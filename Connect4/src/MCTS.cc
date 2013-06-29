#include <math.h>
#include <numeric>
#include <stdlib.h>
#include <string.h>
#include <omp.h>
#include "MCTS.hh"
#include "Timer.hh"
#include "fmath.hpp"

#define NTHREAD 1

namespace MCTS {
class Node;
static Node *pool = NULL;
int alloc, poolCap = 0;

class Node
{
public:
  Node(const Position &pos) : score(0), visited(0) {
    Status end = pos.status();
    if (end != UNDETERMINED)
      nuntried = 0;
    else
      nuntried = pos.genMoves(moves);
    memset(ch, 0xFF, sizeof ch);
  }
  static void init(size_t cap) {
    poolCap = cap;
    pool = (Node *)malloc(sizeof(Node) * cap);
  }
  void *operator new(size_t) {
    if (alloc == poolCap) {
      poolCap = size_t(poolCap * 1.3);
      pool = (Node *)realloc(pool, sizeof(Node) * poolCap);
    }
    return &pool[alloc++];
  }
  int optChild() const {
    int opt = -1;
    float optScore = -1;
    REP(i, MAXN)
      if (ch[i] >= 0) {
        float score = pool[ch[i]].score * -0.5f / pool[ch[i]].visited +
          sqrt(2 * fmath::log(float(visited)) / pool[ch[i]].visited);
        if (score > optScore) {
          optScore = score;
          opt = i;
        }
      }
    return opt;
  }
  int ch[MAXN];
  Move moves[MAXN];
  int nuntried;
  int score;
  int visited;
};

Move MCTS(const Position &pos, Move lastMove)
{
  Timer timer;
  int n = pos.n, m = pos.m;

  if (pool == NULL)
    Node::init(2000000);
  alloc = 0;
  int root = new Node(pos) - pool;

  int path[200];

  for (int _ = 0; ; _++) {
    int npath = 0, p = root;
    Position pos2 = pos;
    path[npath++] = p;

    // select
    while (pool[p].nuntried == 0) {
      int idx = pool[p].optChild();
      if (idx < 0) break;
      pos2.play(pool[p].moves[idx]);
      p = pool[p].ch[idx];
      path[npath++] = p;
    }

    // expand
    if (pool[p].nuntried > 0)
      for (int i = rand() % n; ; ) {
        if (pool[p].ch[i] == -1 && pool[p].moves[i] != NULL_MOVE) {
          pos2.play(pool[p].moves[i]);
          pool[p].nuntried--;
          pool[p].ch[i] = new Node(pos2) - pool;
          p = pool[p].ch[i];
          path[npath++] = p;
          break;
        }
        if (++i == n) i = 0;
      }

    // play out
    int score[NTHREAD];
#pragma omp parallel for schedule(static) num_threads(NTHREAD)
    for (int i = 0; i < NTHREAD; i++) {
      Status end;
      Move moves[MAXN];
      Position pos3(pos2);
      int n = pos3.genMoves(moves);
      while ((end = pos3.status()) == UNDETERMINED) {
        int i = rand() % n;
        pos3.play(moves[i]);
        if (++moves[i] == pos3.hole && ++moves[i] % 16 >= m)
          moves[i] = moves[--n];
      }
      score[i] = end == DRAW ? 1 : end == (pos3.side2move == pos2.side2move ? WIN : LOSE) ? 2 : 0;
    }
    int delta = std::accumulate(score, score + NTHREAD, 0);

    // backpropagate
    while (npath > 0) {
      p = path[--npath];
      pool[p].score += delta;
      pool[p].visited += NTHREAD;
      delta = 2 * NTHREAD - delta;
    }

    if (_ % 256 == 0 && timer.getElapsedMicrosec() > MAX_TIME) {
      //printf("iter %d pool %d\n", _, poolCap);
      break;
    }
  }

  Move optMove = NULL_MOVE;
  int optVisited = 0;
  REP(i, n)
    if (pool[root].moves[i] != NULL_MOVE) {
      optMove = pool[root].moves[i];
      break;
    }
  REP(i, n)
    if (pool[root].ch[i] >= 0 && pool[pool[root].ch[i]].visited > optVisited) {
      optVisited = pool[pool[root].ch[i]].visited;
      optMove = pool[root].moves[i];
    }
  if ((pos.w | pos.b).test(optMove))
    optMove++;
  if (optMove == 0xFF || (pos.w | pos.b).test(optMove)) {
    printf("bad move %d   %llx,%llx,%llx, %llx,%llx,%llx\n", optMove, pos.w.a[0], pos.w.a[1], pos.w.a[2], pos.b.a[0], pos.b.a[1], pos.b.a[2]);
    for (int i = 0; i < n; i++)
      printf("%d ", pool[root].moves[i]);
    puts("");
  }
  return optMove;
}

Move think(const Position &pos, Move lastMove)
{
  return MCTS(pos, lastMove);
}

struct Destructor
{
  ~Destructor() {
    if (pool)
      free(pool);
  }
} singleton;
}
