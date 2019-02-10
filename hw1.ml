
(*--------------1---------------*)
let rec subset a b =
        match a with
        | [] -> true
        | h :: t ->
        	if List.mem h b then
        		subset t b
        	else false;; 
(*--------------2---------------*)

let rec equal_sets a b =
        (subset a b) && (subset b a);;

(*--------------3---------------*)

let rec uniq d e =
	match d with 
	| [] -> e		
	| h :: t ->
		if not(mem h e) then 
			uniq t (h::e)
			else
			uniq t e

let set_union a b =		
	List.sort compare (uniq (uniq a []) (uniq b [])) ;;


(*--------------4---------------*)

let rec inters d e f =
	match d with 
	| [] -> f		
	| h :: t ->
		if List.mem h e then 
			inters t e (h::f)
		else
			inters t e f;;


let set_intersection a b = 
	List.sort compare (inters (uniq a []) (uniq b []) []);;


(*--------------5---------------*)

let rec comp d e f =
	match d with 
	| [] -> f		
	| h :: t ->
		if not(List.mem h e) then 
			comp t e (h::f)
		else		comp t e f;;

let set_diff a b =
	List.sort compare ( comp (uniq a []) (uniq b[]) [] );;

(*--------------6---------------*)

let rec computed_fixed_point eq f x = 
	if eq x (f x) then x
	else computed_fixed_point eq f (f x) ;;

(*--------------7---------------*)

let rec compute eq f p x e = 
	if (=) (List.length e) p  then List.hd e
		else compute eq f p (f x) ((f x)::e) ;;

let rec computed_periodic_point eq f p x  =
	match p with
	| 0 -> x
	| _ -> 
		if eq x (compute eq f p x []) then x
		else computed_periodic_point eq f p (f x) ;;

(*--------------8---------------*)

let rec way s p x e =
	if not (p x) then e
	else way s p (s x) (x::e);;

let while_away s p x = 
	List.sort compare (way s p x []);;

(*--------------9---------------*)

let rec pair a e = 
	if (=) (List.length e) (fst a) then e
	else pair a ((snd a)::e);;

let rec decode lp mp =
	match lp with
	| [] -> mp
	| h:: t -> decode t (List.append mp (pair h []));;

let rle_decode lp =
	decode lp [];;



(*--------------10---------------*)


(*------1-------*)
(* basic type definition *)
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal;;


(*------2-------*)
(* meant to handle rhs for basic run of terminal types - takes in a symbol, outputs true if terminal, false if nonterminal*)
let term symb = 
	match symb with
	| T (_) -> true
	| N (_) -> false ;;

(*------3-------*)
(*takes in list of rules  - rs, which should be snd g - then outputs the initial 'goodList', a list of approved rules*)
let rec first_run rs goodList =
	match rs with
	| [] -> goodList
	| h::t ->
		if List.for_all term (snd h) then first_run t (h::goodList)
		else first_run t goodList;;

(*------4-------*)
(*takes in a list of rules - again, snd g - , then outputs an initial 'goodKey', a list of lhs.  Meant to be run on goodList*)
let rec keyword rs goodKey =
	match rs with 
	| [] -> goodKey
	| h::t -> keyword t ((N (fst h))::goodKey);;

(*------5-------*)
let non symb goodKey =
	if (List.mem symb goodKey || term symb) then true else false;;

(*------6-------*)

(*pass in the rhs of a rule, to check if each member of rhs is valid.  Output true if total rule is valid*)
let rec validrule symblist goodKey =
	match symblist with
	| [] -> true
	| h::t -> if (non h goodKey) then true && (validrule t goodKey) else false;;

(*------7-------*)

(*Check if a rule is valid, if so add its rhs to goodKey, add the rule itself to goodList*)
let rec app rs goodKey goodList =
	match rs with
	| [] -> goodList
	| h::t -> if validrule (snd h) goodKey then app t ((N (fst h))::goodKey) (h::goodList) 
else app t goodKey goodList;;


(*------8-------*)

let rec runthrough app rs goodKey goodList =
	if equal_sets goodList (app rs goodKey goodList) then goodList
	else runthrough app rs (keyword (app rs goodKey goodList) goodKey) (app rs goodKey goodList);;

(*------9-------*)
let rec reverser goodList original sorted =
	match original with 
	| [] -> sorted
	| h::t -> 
		if List.mem h goodList then reverser goodList t (h::sorted) 
		else reverser goodList t sorted;;

(*------10-------*)
let filter_blind_alleys g = ((fst g),(List.rev( reverser (uniq (((runthrough app (snd g) (keyword (first_run (snd g) []) []) (first_run (snd g) [])))) []) (snd g) [])));;


let filter_blind_alleys g =

	let goodList = first_run (snd g) [] in
	let goodKey = keyword goodList [] in

((fst g),(List.rev( reverser (uniq (runthrough app (snd g) goodKey goodList) []) (snd g) [])));;

	runthrough app (snd g) goodKey goodList;;


