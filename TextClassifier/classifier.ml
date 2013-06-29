open Core
open ExtList
open Lacaml.D
open Libsvm

module StringSet = Set.Make(String)
module SM = Map.Make(String)

module type Classifier = sig
  val train : string DynArray.t SM.t -> unit
  val classify : string -> string
end

let (|>) x f = f x

let flip f x y = f y x

let find_default m k def =
  try
    SM.find k m
  with Not_found ->
    def

let process_file filename f =
  let chan = open_in filename in
  try
    while true; do
      input_line chan |> f
    done;
  with End_of_file ->
    close_in chan

let read_words filename f =
  let islower c = let c = Char.chr c in 'a' <= c && c <= 'z' in
  let chan = open_in filename in
  let read_char () =
    try
      input_char chan |> Char.lowercase |> Char.code
    with End_of_file -> (
      close_in chan;
      -1
    ) in
  let rec go flag s = match flag with
    | false ->
        let c = read_char () in
        if c = -1 then
          ()
        else if islower c then
          go true (String.make 1 (Char.chr c))
        else
          go false s
    | true ->
        let c = read_char () in
        if c = -1 then
          f s
        else if islower c then
          go true (s ^ String.make 1 (Char.chr c))
        else (
          f s;
          go false s
        ) in
  go false ""

module Idf = struct
  let ndoc = ref 0
  let vocab = ref SM.empty
  let idf = ref SM.empty
  let categories = DynArray.create ()
  let cat2id = ref SM.empty

  let calc_idf cat2filenames =
    let cat_id = ref 0 in
    let vocab_id = ref 0 in
    ndoc := SM.fold (fun _ filenames sum ->
      sum + DynArray.length filenames) cat2filenames 0;
    SM.iter (fun cat filenames ->
      DynArray.add categories cat;
      cat2id := SM.add cat !cat_id !cat2id;
      incr cat_id;
      let added = ref StringSet.empty in
      DynArray.iter (fun filename ->
        read_words filename (fun word ->
          if SM.mem word !vocab |> not then (
            vocab := SM.add word !vocab_id !vocab;
            incr vocab_id
          );

          (* document frequency *)
          if StringSet.mem word !added |> not then (
            added := StringSet.add word !added;
            let dfv = find_default !idf word 0.0 in
            idf := SM.add word (dfv +. 1.0) !idf
          );
        );
      ) filenames;
    ) cat2filenames;
    (* inverse document frequency *)
    idf := SM.map (fun dfv -> log (float_of_int !ndoc /. dfv)) !idf;
    print_endline "meow"
end

module NaiveBayes = struct
  include Idf

  let prior = ref SM.empty

  let prob = ref SM.empty

  let train cat2filenames =
    let ncat = SM.fold (fun _ _ acc -> acc + 1) cat2filenames 0 in

    calc_idf cat2filenames;

    prior := SM.map (fun filenames ->
      (float_of_int (DynArray.length filenames) /.  float_of_int ncat))
      cat2filenames;

    SM.iter (fun cat filenames ->
      let cat_prob = ref SM.empty in
      DynArray.iter (fun filename ->
        print_endline filename;
        read_words filename (fun word ->
          let cnt = find_default !cat_prob word 0.0 in
          cat_prob := SM.add word (cnt +. 1.0) !cat_prob;
        );
        print_endline filename;
      ) filenames;
      let tot = SM.fold (fun _ c acc -> acc +. c +. 1.0) !cat_prob 0.0 in
      SM.iter (fun word _ ->
        let cnt = find_default !cat_prob word 0.0 in
        cat_prob := SM.add word ((cnt +. 1.0) /. tot) !cat_prob
      ) !vocab;
      prob := SM.add cat !cat_prob !prob;
    ) cat2filenames

  let classify filename =
    let score = prior in
    read_words filename (fun word ->
      if SM.mem word !vocab then
        SM.iter (fun cat cat_prob ->
          let value = SM.find cat !score in
          let p = SM.find word cat_prob in
          score := SM.add cat (value +. log p) !score
        ) !prob
    );
    SM.fold (fun cat v ((optc, optv) as opt) ->
      if v > optv then (cat, v) else opt
    ) !score ("", neg_infinity) |> fst
end

module SVM = struct
  include Idf

  open Lacaml.D

  let model =
    let x = Mat.of_array
        [|
          [| 0.; 0. |];
          [| 0.; 1. |];
          [| 1.; 0. |];
          [| 1.; 1. |];
        |]
      in
    let targets = Vec.of_array [| 0.; 1.; 1.; 0. |] in
    let problem = Svm.Problem.create ~x ~y:targets in
    ref (Svm.train ~kernel:`RBF problem)

  let features = ref SM.empty

  let train cat2filenames =
    calc_idf cat2filenames;

    (*Printf.printf "%d \n" !ndoc ;*)
    SM.fold (fun k v acc -> (k, v) :: acc) !idf [] |>
      List.sort ~cmp:compare |> List.take 20000 |>
      List.iteri (fun i (word, _) -> features := SM.add word i !features);

    let nfeatures = SM.fold (fun _ _ acc -> acc + 1) !features 0 in
    (*Printf.printf "%d %d\n" !ndoc (Hashtbl.length features);*)

    let x = Mat.make !ndoc nfeatures 0.0 in
    let y = Vec.create !ndoc in
    let row = ref 0 in

    SM.iter (fun cat filenames ->
      let cat_id = SM.find cat !cat2id in
      DynArray.iter (fun filename ->
        incr row;
        read_words filename (fun word ->
          if SM.mem word !features then (
            let id = SM.find word !features + 1 in
            let idfv = SM.find word !idf in
            x.{!row, id} <- x.{!row, id} +. idfv
          )
        );
        y.{!row} <- float_of_int cat_id
      ) filenames
    ) cat2filenames;

    let problem = Svm.Problem.create ~x ~y in
    model := Svm.train ~kernel:`RBF problem

  let classify filename =
    let nfeatures = SM.fold (fun _ _ acc -> acc + 1) !features 0 in
    let x = Mat.make 1 nfeatures 0.0 in
    read_words filename (fun word ->
      if SM.mem word !features then (
        let id = SM.find word !features + 1 in
        let idfv = SM.find word !idf in
        x.{1, id} <- x.{1, id} +. idfv
      )
    );
    let y = Float.round (Svm.predict !model ~x).{1} |> int_of_float
      |> max 0 |> min (DynArray.length categories) in
    DynArray.get categories y
end

module MakeClassifier (C: Classifier) = struct
  let process_filelist filelist cont =
    let cat = ref "" in
    process_file filelist (fun line ->
      try
        Scanf.sscanf line "# %s" (fun cat2 -> cat := cat2; cont !cat "")
      with Scanf.Scan_failure e ->
        cont !cat line
      )

  let train filelist =
    let cat2filenames = ref SM.empty in
    process_filelist filelist (fun cat filename ->
      if filename = "" then
        cat2filenames := SM.add cat (DynArray.create ()) !cat2filenames
      else
        let arr = SM.find cat !cat2filenames in
        DynArray.add arr filename
    );
    C.train !cat2filenames

  let classify filelist =
    let correct = ref 0 in
    let tot = ref 0 in
    process_filelist filelist (fun cat filename ->
      if filename = "" then
        Printf.printf "# %s\n" cat
      else (
        tot := !tot + 1;
        if C.classify filename = cat then
          correct := !correct + 1
      )
    );
    Printf.printf "precision: %f\n" (float_of_int !correct /. float_of_int !tot)
end

module NBClassifier = MakeClassifier(NaiveBayes);;

NBClassifier.train "train.list";;

NBClassifier.classify "classify.list";;

(*module SVMClassifier = MakeClassifier(SVM);;*)

(*SVMClassifier.train "train.list";;*)

(*SVMClassifier.classify "classify.list";;*)
