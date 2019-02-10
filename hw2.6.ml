open List;;

type ('terminal, 'nonterminal) symbol =
    | T of 'terminal
    | N of 'nonterminal;;

(*meant to grab an alternate list for 1 nt*)
let rec altlist nt rules alt = 
	match rules with 
	| [] -> alt
	| h::t -> 
		if (=) (fst h) nt then altlist nt t ((snd h):: alt)
		else altlist nt t alt ;;


(*the one with the correct order*)
let rec convert_grammar gram1 =
	match gram1 with
	| (ss, rules) -> (ss, function nt -> (List.rev (altlist nt rules [])));;

type awksub_nonterminals =
  | Expr | Term| Lvalue | Incrop | Binop | Num;;

let awksub_rules =
[ Expr, [N Term; N Binop ; N Expr];
  Expr, [N Term];
    Term, [N Num];
    Term, [N Lvalue];
    Term, [N Incrop; N Lvalue];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Num, [T"0"];
    Num, [T"1"];
    Num, [T"2"];
    Num, [T"3"];
    Num, [T"4"];
    Num, [T"5"];
    Num, [T"6"];
    Num, [T"7"];
    Num, [T"8"];
    Num, [T"9"]];;

let awksub_grammar = Expr, awksub_rules;;

let gram1 = convert_grammar  awksub_grammar;;

let accept_all derivation string = Some (derivation, string);;


let term produc sym =
	match sym with
	| T (x) -> []
	| N (x) -> (produc x);;

let safe produc sym =
  if (=) (term produc sym) [] then []
  else hd (term produc sym);;

let pro_express sym =
  match sym with
  |T (x) -> x
  |N (x) -> x;;


let rec basic_deriv produc sym list deriv bool accept frag =
	match frag with 
	|[] -> accept deriv [] (*in this case, then clearly the derivation is done and 
		there's no more suffix*)	
	| head::tail -> (*we're going through each fragment now*)
		match sym with 
		| T (x) -> 
			if (=) (T head) (sym) then Some ( deriv, tail) else None
		| N (x) -> match list with
			| [] -> None 
			| h::t -> 
				match (grab produc x h (deriv @[x, h]) bool accept frag) with 
					| None -> (basic_deriv produc sym t deriv bool
						accept frag) 
					| Some (a, b) -> if (=) bool true 
                                                            then (accept a b) else Some (a, tail)


and grab produc expr rule deriv bool accept frag =
	match rule with
	| [] -> accept deriv [] 
	| h::t ->
		match (basic_deriv produc h (term produc h) deriv false accept frag) with
		| None -> None 
		| Some (x, y) -> if (bool && not ((=) t []))  then (**) grab produc expr t x bool accept y(**) (* (basic_deriv produc (hd t) (term produc (hd t)) x bool accept y)*)
      else grab produc expr t x false accept frag ;;




let test1 =
  (basic_deriv (snd gram1) ( N (fst gram1)) ((snd gram1) (fst gram1)) [] true accept_all ["3"]) = Some([ Expr, [N Term];
  Term, [N Num];
  Num, [T "3"] ], []);;

let test2 = basic_deriv (snd gram1) ( N (fst gram1)) ((snd gram1) (fst gram1)) [] true accept_all ["3"; "+"; "4"];;



let matchel gram =
	basic_deriv (snd gram1) ( N (fst gram1)) ((snd gram1) (fst gram1)) [] true ;;

let test3 = matchel gram1 accept_all ["3"; "+"; "4";];;

let test4 = matchel gram1 accept_all ["3"];;

(*let test5 = parse_prefix gram1 accept_all ["3"; "+"; "4"];*)
