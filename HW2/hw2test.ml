
let accept_all derivation string = Some (derivation, string)  ;;

type awksub_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num    ;;

let awkish_grammar =
  (Expr,
   function
     | Expr ->
         [[N Term; N Binop; N Expr];
          [N Term]]
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
      [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]])  ;;

let test_1 =
    ((parse_prefix awkish_grammar accept_all ["("; "$"; "9"; ")"])
        = Some
            ([(Expr, [N Term]); (Term, [T "("; N Expr; T ")"]); (Expr, [N Term]);
              (Term, [N Lvalue]); (Lvalue, [T "$"; N Expr]); (Expr, [N Term]);
              (Term, [N Num]); (Num, [T "9"])],
                []))  ;;

type gram_nt =
    | Sentence | Stmt | Period | Adj | Noun | Verb  ;;

let new_grammar =
    (Sentence,
        [
            Sentence, [N Stmt; T","; N Sentence];
            Sentence, [N Stmt; N Period];
            Stmt, [N Adj; N Noun; N Verb; N Stmt];
            Stmt, [N Adj; N Noun];
            Stmt, [N Noun];
            Period, [T "."];
            Adj, [T "fast"];
            Adj, [T "strong"];
            Noun, [T "player"];
            Noun, [T "refree"];
            Noun, [T "coach"];
            Verb, [T "shoots"];
            Verb, [T "yells"];
            Verb, [T "passes"];
            Noun, [T "ball"];
            Noun, [T "basket"];
            Noun, [T "shot"];
            Adj, [T "clutch"];
            Adj, [T "good"]]    ) ;;

let test_grammar = convert_grammar new_grammar  ;;

let test_2 = 
    ((parse_prefix test_grammar accept_all ["strong"; "player"; "shoots"; "clutch"; "shot"; "."])
        = Some
            ([(Sentence, [N Stmt; N Period]); (Stmt, [N Adj; N Noun; N Verb; N Stmt]);
              (Adj, [T "strong"]); (Noun, [T "player"]); (Verb, [T "shoots"]);
              (Stmt, [N Adj; N Noun]); (Adj, [T "clutch"]); (Noun, [T "shot"]);
              (Period, [T "."])],
                []))  ;;

