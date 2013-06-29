{-# LANGUAGE CPP, FlexibleInstances, TypeSynonymInstances, ViewPatterns #-}
{-
Input "123804765"
stands for

1 2 3
8 0 4
7 6 5

Add -g to generate code for graphviz
 -}

module Main (main, State, moves, search, bfs, astar, heuristic) where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Function
import qualified Data.Sequence as Seq
import qualified Data.Map as M
import qualified Data.MultiSet as SS
import System.Environment
import System.IO

-- | State is an Int list
type State = [Int]
target = [1,2,3,8,0,4,7,6,5] :: State
target' = fromEnum target

-- Factorials, [1,1,2,6,24,120...]
factorials = 1 : scanl1 (*) [1..]

-- | Lehmor code
instance Enum State where
  -- Encode some state
  fromEnum a = (\(_,_,acc) -> acc) $ foldr (\x (i,l,acc) ->
      (i+1,x:l,acc+(factorials!!i)*length (filter (<x) l))) (0,[],0) a
  -- Decode from Lehmor code
  toEnum acc = unfoldr (\(i,l,acc) ->
      if i < 0 then Nothing
      else let (q,r) = acc `divMod` (factorials !! i)
               x = l !! q
           in Just (x, (i-1,delete x l,r))
          ) (8,[0..8],acc)

-- | Generate neighbor states
moves :: State -> [State]
moves s = [ map (\x -> if x == 0 then s!!pos' else if x == s!!pos' then 0 else x) s
          | d <- [-1,3,1,-3]
          , not $ pos `mod` 3 == 0 && d == (-1)
          , not $ pos `mod` 3 == 2 && d == 1
          , let pos' = pos + d
          , not $ pos' < 0 || pos' >= 9
          ]
  where
    pos = fromJust $ findIndex (==0) s

-- Solve a puzzle using chosen strategy (bfs / a*)
solve :: (State -> M.Map Int Int) -> State -> Handle -> IO ()
solve strategy src hout = do
    let ss = if fromEnum src == target' then M.singleton 0 (-1) else strategy src
    -- the puzzle is solvable iff the permutations of src & target have the same parity
    if parity (delete 0 src) /= parity (delete 0 target)
       then putStrLn "no solution"
       else getArgs >>= \args -> if (elem "-g" args)
            then do
                putStrLn "digraph {"
                forM_ (nub $ M.keys ss) $ \s ->
                    putStrLn $ show s ++ " [shape=record" ++
                        (if s == fromEnum src
                         then ",style=filled,color=orange"
                         else if s == fromEnum target
                              then ",style=filled,color=orchid"
                              else "") ++ ",label=\""++label s++"\"];"
                forM_ (filter ((/=fromEnum src) . fst) $ M.toList ss) $ \(s,p) ->
                    putStrLn $ show p ++ "->" ++ show s ++ ";"
                putStrLn "}"
            else do
              output (fromEnum target) 0 ss
  where
    putStrLn = hPutStrLn hout
    print = hPrint hout
    groupBy3 = groupBy ((/=) `on` fst) . zip (cycle [1..3]) . (toEnum :: Int -> State)
    label = intercalate "|" . map (('{':).(++"}") . intersperse '|' . concatMap show . map snd) . transpose . groupBy3
    -- parity of permutation
    parity = even . snd . foldr (\x (l,acc) -> (x:l,acc+length(filter(<x)l))) ([],0)
    output s l m = do
      if s == fromEnum src
         then print l
         else output (m M.! s) (l+1) m
      putStrLn ""
      mapM_ (putStrLn . intersperse  ' ' . concatMap show . map snd) . groupBy3  $ s

-- | Framework of a search algorithm
search :: (t -> (s, t)) -- ^ Extract current node from open set
       -> (s -> State) -- ^ Get the state represented by current node
       -> ((s, t) -> [State] -> t) -- ^ Merge the old open set with new candidate nodes
       -> t -- ^ Open set
       -> M.Map Int Int -- ^ Closed set ((old_state, new_state) pairs)
       -> M.Map Int Int -- ^ Result (closed set)
search extract transform merge open closed
    -- Search algorithm will terminate if the target is reached
    | isJust $ find (==target') suc' = closed'
    -- Otherwise repeat the search with new open/closed sets
    | otherwise = search extract transform merge (merge (h,open') suc) closed'
  where
    -- Extract the head from open set
    (h,open') = extract open
    -- Generate unvisited neighbor moves
    suc = filter (not . flip M.member closed . fromEnum) . moves $ transform h
    suc' = map fromEnum suc
    -- New closed set is the union of the old closed set and (new_state, old_state) pairs
    closed' = M.union closed . M.fromList . zip suc' . repeat . fromEnum $ transform h

bfs :: State -> M.Map Int Int
bfs src = search extract id merge (Seq.singleton src) $ M.singleton (fromEnum src) (-1)
  where
    -- Extract the head state from the queue
    extract = (\(h Seq.:< t) -> (h, t)) . Seq.viewl
    -- Concatenate the two sequences
    merge (h,open') suc = open' Seq.>< Seq.fromList suc

astar :: State -> M.Map Int Int
astar src = search extract snd merge (SS.singleton (heuristic src, src)) $ M.singleton (fromEnum src) (-1)
  where
    -- Extract the state with minimum heuristic
    extract = fromJust . SS.minView
    -- Merge the two ordered sets
    merge ((c,p),open') suc = SS.union open' $ SS.fromList $ map (\q -> (c - heuristic p + 1 + heuristic q, q)) suc

-- | A consistent and admissible heuristic: the sum of Manhattan distances of tiles from their goal positions
heuristic s = sum $ map (\i -> distance (index i s) (index i target)) [1..8]
  where
    index x = fromJust . findIndex (== x)
    distance p q = abs (x1-x2) + abs (y1-y2)
      where
        (x1,y1) = p `divMod` 3
        (x2,y2) = q `divMod` 3

main = do
  args <- getArgs
  let files = filter ((/= '-') . head) args
  line <- fmap (filter (flip elem ['0'..'9'])) (if null files then getContents else readFile $ head files)
  h <- if length files > 1 then openFile (last files) WriteMode else return stdout
#ifdef BFS
  solve bfs (map (read . return) line) h
#else
  solve astar (map (read . return) line) h
#endif
  hClose h
