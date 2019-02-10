
(*--------------1f---------------*)

let my_subset_test0 = subset [] []
let my_subset_test1 = not(subset [1;2;3] [])
let my_subset_test2 = subset [3;1;3] [1;2;3]
let my_subset_test3 = not (subset [1;3;314253;134253;5643] [4;1;3;3124253;34253;314253;134253;56434;])

(*--------------2---------------*)
let my_equal_sets_test0 = equal_sets [] []
let my_equal_sets_test1 = not (equal_sets [1;2;3] [])
let my_equal_sets_test2 = equal_sets [3;1;3] [3;3;1]

(*--------------3---------------*)

let my_set_union_test0 = equal_sets (set_union [2;2;2;2;2;2;2;2;1;3] [1;2;3]) [1;2;3]
let my_set_union_test1 = equal_sets (set_union [] [1;2;3]) [1;2;3]
let my_set_union_test2 = equal_sets (set_union [2;2;2;2;2;2;2;2;1;3] [2;1;2;2;1;3;2]) [1;2;3]

(*--------------4---------------*)
let my_set_intersection_test0 =
  equal_sets (set_intersection [] []) []
let my_set_intersection_test1 =
  equal_sets (set_intersection [3;3;3;3;1;3] [1;2;2;3;3]) [1;3]
let my_set_intersection_test2 =
  equal_sets (set_intersection [1;2;3;4] [5;6;7;8;]) []

(*--------------5---------------*)

let my_set_diff_test0 = equal_sets (set_diff [1;3;5;6] [1;4;3;1]) [5;6]
let my_set_diff_test1 = equal_sets (set_diff [4;3;1;1;3] []) [1;3;4]

(*--------------6---------------*)

let my_computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x / (5)) 1000000000 = 0

(*--------------7---------------*)

let my_computed_periodic_point_test0 =
  computed_periodic_point (=) (fun x -> x / 2) 0 (2) = 2
let my_computed_periodic_point_test1 =
  computed_periodic_point (=) (fun x -> x / 2) 3 (2) = 0

(*--------------8---------------*)

let my_while_away_test0 = equal_sets (while_away((+) 4) ((>) 10) 0) [0; 4; 8]

(*--------------9---------------*)

let my_rle_decode_test0 = equal_sets (rle_decode [2,0; 2,6]) [0; 0; 6; 6]

(*--------------10---------------*)


type sleep_nonterminals =
  | Awake | Insomniac | Narcoleptic |Morning| Night | Sleep

let sleep_rules =
   [Awake, [N Narcoleptic];
    Awake, [N Night];
    Awake, [N Insomniac; N Awake];
    Insomniac, [N Night];
    Insomniac, [N Morning];
    Insomniac, [N Sleep; N Night];
    Narcoleptic, [N Night; N Sleep];
    Morning, [T"Bird"];
    Morning, [T"Dove"];	
    Night, [T"Mid"];
    Night, [T"Owl"]]

let sleep_grammar = Awake, sleep_rules

let my_filter_blind_alleys_test0 =
filter_blind_alleys sleep_grammar = 
(Awake, [
    Awake, [N Night];
    Awake, [N Insomniac; N Awake];
    Insomniac, [N Night];
    Insomniac, [N Morning];
    Morning, [T"Bird"];
    Morning, [T"Dove"];	
    Night, [T"Mid"];
    Night, [T"Owl"]])
