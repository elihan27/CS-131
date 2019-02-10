
open List;;

type ('terminal, 'nonterminal) symbol =
    | T of 'terminal
    | N of 'nonterminal;;


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

let test_1 =
  (parse_prefix gram1 accept_all ["3"]) = Some([ Expr, [N Term];
  Term, [N Num];
  Num, [T "3"] ], []);;



type hope_nonterminals =
|Despair | Hope | Cake | Life;;

let hope_grammar =
(Despair, function
	| -


type hope_nonterminals =
  | Hope | Despair | Cake ;;

let hope_grammar =
  (Hope,
   function
     | Expr ->
         [ [N Hope ; N Despair; N Cake ];
          
     | Term ->
	 [[N Num];
	  [N Lvalue];
	  [N Incrop; N Lvalue];
	  [N Lvalue; N Incrop];
	  [T"("; N Expr; T")"]]
     | Lvalue ->
	 [[T"$"; N Expr]]
     | Incrop ->
	 [[T"++"];
	  [T"--"]]
     | Binop ->
	 [[T"+"];
	  [T"-"]]
     | Num ->
	 [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
	  [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]])






)


let hope_rules =
[
	Cake, [T"Chocolate"; T]Expr, [N Term; N Binop ; N Expr]

]







let test2 = basic_deriv (snd gram1) ( N (fst gram1)) ((snd gram1) (fst gram1)) [] true accept_all ["3"; "+"; "4"];;



let matchel gram =
	basic_deriv (snd gram1) ( N (fst gram1)) ((snd gram1) (fst gram1)) [] true ;;

let test3 = matchel gram1 accept_all ["3"; "+"; "4";];;

let test4 = matchel gram1 accept_all ["3"];;

(*let test5 = parse_prefix gram1 accept_all ["3"; "+"; "4"];*)


let sleep_rules =
   [Awake, [N Narcoleptic];
    Awake, [N Night];
    Awake, [N Insomniac; Awake];
    Insomniac, [N Night];
    Insomniac, [N Morning];
    Insomniac, [N Sleep; N Night];
    Narcoleptic, [N Night; N Sleep];
    Morning, [T"Bird"];
    Morning, [T"Dove"];	
    Night, [T"Mid"];
    Night, [T"Owl"]]





let test_1




let test_2


