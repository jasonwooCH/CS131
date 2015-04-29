let my_subset_test0 = subset [] []
let my_subset_test1 = subset [1;2;3;4;5;6;7;8;9] [1;3;5;7;9]
let my_subset_test2 = subset [1;2;9;3] [3;2;1;9]

let my_equal_sets_test0 = equal_sets [1;2;9;3] [3;2;1;9]
let my_equal_sets_test1 = not (equal_sets [1;3;5] [1;3;5;7])

let my_set_union_test0 = equal_sets (set_union [] [1;3;5]) [1;3;5]
let my_set_union_test1 = equal_sets (set_union [1;3;5] [2;4;6]) [1;2;3;4;5;6]

let my_set_intersection_test0 =
	equal_sets (set_intersection [1;3;5] [2;4;6]) []
let my_set_intersection_test1 =
	equal_sets (set_intersection [1;2;3] [4;2;1]) [1;2]

let my_set_diff_test0 = equal_sets (set_diff [1;3;5] [2;4;6]) [1;3;5]
let my_set_diff_test1 = equal_sets (set_diff [5;4;3] [3;2;1]) [5;4]
let my_set_diff_test2 = equal_sets (set_diff [1;2;3] []) [1;2;3]

let my_computed_fixed_point_test0 =
	computed_fixed_point (=) (fun x -> x * x) 0 = 0
let my_computed_fixed_point_test1 =
	computed_fixed_point (=) (fun x -> x / x) 100 = 1

let my_computed_periodic_point_test0 =
	computed_periodic_point (=) (fun x -> x * x) 0 3 = 3
let my_computed_periodic_point_test1 =
	computed_periodic_point (=) (fun x -> x *. x -. 1.) 2 0.5 = -1.

type awksub_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num

let awksub_rules =
   [Expr, [T"("; N Expr; T")"];
    Expr, [N Num];
    Expr, [N Expr; N Binop; N Expr];
    Expr, [N Lvalue];
    Expr, [N Incrop; N Lvalue];
    Expr, [N Lvalue; N Incrop];
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
    Num, [T"9"]]

let my_filter_blind_alleys_test0 =
	filter_blind_alleys (Expr, awksub_rules) = (Expr, awksub_rules)

let my_filter_blind_alleys_test1 =
	filter_blind_alleys (Expr, List.tl (List.tl (List.tl (List.tl awksub_rules)))) =
    filter_blind_alleys (Expr, List.tl (List.tl awksub_rules))
    
