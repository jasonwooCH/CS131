type ('nonterminal, 'terminal) symbol =
	| N of 'nonterminal
	| T of 'terminal 	;;

(* 	production function to create an alternative list for a given NT symbol *)
let rec prod_fxn rules nt_symbol =
	match rules with
		[] -> []
	| 	(h::t) -> match h with
					  (a,b) -> if a == nt_symbol
							   then [b] @ (prod_fxn t nt_symbol)
							   else prod_fxn t nt_symbol	;;

(*	prod_fxn will recursively create an alternative list and pair with start NT symbol *)
let convert_grammar gram1 =
	match gram1 with
		(a,b) -> (a, prod_fxn b)	;;

(*	This function recursively checks the rules to find a terminal symbol match 		*
 *	for the elements of the fragment. If there are no rules, it will be passed		*
 *	to the acceptor. Otherwise, it'll check T or NT. If NT, check recursively until *
 *	T found. If T, check if equal with the element.		   							*)
let rec match_terminal gram rules accept deriv frags =
	match rules with
		[] -> accept deriv frags
	| 	h::t -> match frags with
				[] -> None
			| 	head::tail -> match h with
								 (N nt) -> let acceptor = (match_terminal gram t accept) in
												(match_both gram nt acceptor deriv frags)
							  |  (T ter) -> if head = ter
							  				then (match_terminal gram t accept deriv tail)
							  				else None

(* This function checks for the NT argument in the rules. If found, it is returned. *
 *	If not, it recursively checks the rest, until there is none left,				*
 *	where it will end with returning None.										    *)
and match_NT gram nt_symbol rules accept deriv frags =
	match rules with
		[] -> None
	|   h::t -> let new_deriv = (deriv@[(nt_symbol,h)]) in
				match (match_terminal gram h accept new_deriv frags) with
				    None -> (match_NT gram nt_symbol t accept deriv frags) 	
				| 	result -> result 

(* 	This function starts the mutual recursive function of matching T and NT with *
 *	given grammar, acceptor, and fragment										 *)
and match_both gram nt_symbol accept deriv frags =
	let rules = gram nt_symbol in
		(match_NT gram nt_symbol rules accept deriv frags)	;;

(*	*	*	*	*	*
 *	 PARSE_PREFIX 	*
 *	*	*	*	*	*)
let rec parse_prefix gram accept frags =
	match gram with
		(nt_symbol, grams) -> (match_both grams nt_symbol accept [] frags)	;;
