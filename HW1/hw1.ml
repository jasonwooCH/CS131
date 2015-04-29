(*	*	*	*	*	*	*
	WARM UP FUNCTIONS
 *	*	*	*	*	*	*)

let rec subset a b =
	match a with
		[] -> true
	| 	h::t -> (List.mem h b) && subset t b ;;

let equal_sets a b =
	(subset a b) && (subset b a) ;;

let set_union a b =
	a @ b ;;

let rec set_intersection a b = 
	match a,b with
		[], _ -> []
	|	_, [] -> []
	|  	ha::ta, hb::tb ->
			if ha = hb
			then ha::(set_intersection ta b)
			else
				if (set_intersection [ha] tb) = []
				then set_intersection ta b
				else (set_intersection [ha] tb) @ (set_intersection ta b) ;;

let rec set_diff a b = 
	match a, b with
		[], _ -> []
	| 	l, [] -> l
	|  	ha::ta, hb::tb ->
			if ha = hb
			then set_diff ta b
			else
				if (set_diff [ha] tb) = [ha]
				then ha::(set_diff ta b)
				else set_diff ta b ;; 

let rec computed_fixed_point eq f x =
	if eq x (f x)
	then x
	else computed_fixed_point eq f (f x) ;;

let rec compound f p x =
	if p = 0
	then x
	else f (compound f (p - 1) x) ;;

let rec computed_periodic_point eq f p x =
	if eq x (compound f p x)
	then x
	else computed_periodic_point eq f p (f x) ;;


(*	*	*	*	*	*	*	*	*	*	*	*
	AUX FUNCTIONS AND FILTER_BLIND_ALLEYS
 *	*	*	*	*	*	*	*	*	*	*	*)

type ('nonterminal, 'terminal) symbol = 
	|    N of 'nonterminal
	|    T of 'terminal ;;

(* 	checks if a rule contains only T elements
   	ex. [T"("; T")"] would return true		*)
let rec is_terminal r =
	match r with
		[] -> true
	| 	h::t -> match h with 
					(N a) -> false
				| 	(T b) -> is_terminal t ;;

(* 	creates a list of nonterminals that leaves with terminals 
   	from here on, we will refer to those elements as TNTs	*)
let rec make_tlist a =
	match a with
		[] -> []
	|	h::t -> if is_terminal (snd h)
				then (fst h)::(make_tlist t)
				else make_tlist t ;;

(* 	checks if a rule contains only terminal/TNTs	
   	ex. if Num = TNT then [T"s"; N Num] would return true *)
let rec check_tlist x y = 
	match x with 
		[] -> true
	| 	h::t -> match h with
					(N a) -> if subset [a] y
							 then check_tlist t y
							 else false
				| 	(T b) -> check_tlist t y ;;

(* 	adds to the TNT list nonterminals whose rule contains
   	only terminals and/or TNTs							*)
let rec addto_tlist u w = (* w = make_tlist u *)
	match u with 
		[] -> w
	| 	h::t -> if (check_tlist (snd h) w) && not(subset [fst h] w)
				then (fst h)::w
				else addto_tlist t w ;;

(*	perform addto_tlist until all TNTs have been added *)
let rec finish_tlist u w =
	if w <> addto_tlist u w
	then addto_tlist u w
	else w ;;

(*	creat the final list of TNTs  *)
let final_tlist t =
	finish_tlist t (addto_tlist t (make_tlist t));;

(*	for each nonterminal, check_tlist its rule with TNT list. 
	recursively add those that satisfy						*)
let rec build_grammar g l = 
	match g with
		[] -> []
	| 	h::t -> match h with
				(a,b) -> if check_tlist b l 
						 then (a,b)::build_grammar t l
						 else build_grammar t l ;;

(*	Putting the aux functions altogether
	We eliminate rules that have no terminal leaves *)
let filter_blind_alleys g =
	match g with
		(a,b) -> (a,build_grammar b (final_tlist b)) ;;
