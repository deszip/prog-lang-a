(* 1. Write a function only_capitals that takes a string list and returns a string list that has only the strings in the argument that start with an uppercase letter. Assume all strings have at least 1 character. Use List.filter, Char.isUpper, and String.sub to make a 1-2 line solution. *)

val only_capitals = List.filter (fn x=> (Char.isUpper o String.sub) (x, 0))

val only_capitals_test_1 = only_capitals ["A","B","C"] = ["A","B","C"]
val only_capitals_test_2 = only_capitals [] = []
val only_capitals_test_3 = only_capitals ["foo", "bar", "baz"] = []
val only_capitals_test_4 = only_capitals ["foo", "bar", "Baz"] = ["Baz"]


(* 2. Write a function longest_string1 that takes a string list and returns the longest string in the list. If the list is empty, return "". In the case of a tie, return the string closest to the beginning of the list. Use foldl, String.size, and no recursion (other than the implementation of foldl is recursive). *)

val longest_string1 = List.foldl (fn (x, acc) => if String.size x > String.size acc then x else acc) ""


val longest_string1_test_1 = longest_string1 ["A","bc","C"] = "bc"
val longest_string1_test_2 = longest_string1 ["ba","bc","C"] = "ba"
val longest_string1_test_3 = longest_string1 [] = ""
val longest_string1_test_4 = longest_string1 ["", "bc"] = "bc"


(* 3. Write a function longest_string2 that is exactly like longest_string1 except in the case of ties it returns the string closest to the end of the list. Your solution should be almost an exact copy of longest_string1. Still use foldl and String.size. *)

val longest_string2 = List.foldl (fn (x, acc) => if String.size x >= String.size acc then x else acc) ""

val longest_string2_test_1 = longest_string2 ["A","bc","C"] = "bc"
val longest_string2_test_2 = longest_string2 ["ba","bc","C"] = "bc"


(* 4 *)

fun longest_string_helper f = List.foldl (fn (x, acc) => if f(String.size x, String.size acc) then x else acc) ""
val longest_string3 = longest_string_helper (fn (a, b) => a > b)
val longest_string4 = longest_string_helper (fn (a, b) => a >= b)

val longest_string3_test_1 = longest_string1 ["A","bc","C"] = "bc"
val longest_string3_test_2 = longest_string1 ["ba","bc","C"] = "ba"
val longest_string3_test_3 = longest_string1 [] = ""
val longest_string3_test_4 = longest_string1 ["", "bc"] = "bc"

val longest_string4_test_1 = longest_string2 ["A","bc","C"] = "bc"
val longest_string4_test_2 = longest_string2 ["ba","bc","C"] = "bc"


(* 5. Write a function longest_capitalized that takes a string list and returns the longest string in the list that begins with an uppercase letter, or "" if there are no such strings. Assume all strings have at least 1 character. Use a val-binding and the ML library’s o operator for composing functions. Resolve ties like in problem 2. *)

val longest_capitalized =  longest_string1 o only_capitals

val longest_capitalized_test_1 = longest_capitalized ["A","bc","C"] = "A"
val longest_capitalized_test_2 = longest_capitalized ["a","a","a"] = ""


(* 6. Write a function rev_string that takes a string and returns the string that is the same characters in reverse order. Use ML’s o operator, the library function rev for reversing lists, and two library functions in the String module. (Browse the module documentation to find the most useful functions.) *)

val rev_string = String.implode o List.rev o String.explode

val rev_string_test_1 = rev_string "abc" = "cba"
val rev_string_test_2 = rev_string "" = ""


(* 7. Write a function first_answer of type (’a -> ’b option) -> ’a list -> ’b (notice the 2 argu- ments are curried). The first argument should be applied to elements of the second argument in order until the first time it returns SOME v for some v and then v is the result of the call to first_answer. If the first argument returns NONE for all list elements, then first_answer should raise the exception NoAnswer. Hints: Sample solution is 5 lines and does nothing fancy. *)

exception NoAnswer
fun first_answer f = fn s => case s of 
														   [] => raise NoAnswer
															 | x::xs => if isSome (f(x))
															 	  				then x
																	  			else first_answer f xs

val first_answer_test_1 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val first_answer_test_2 = (((first_answer (fn x => if x = 6 then SOME x else NONE) [1,2,3,4,5]); false) handle NoAnswer => true)


(* 8. Write a function all_answers of type (’a -> ’b list option) -> ’a list -> ’b list option (notice the 2 arguments are curried). The first argument should be applied to elements of the second argument. If it returns NONE for any element, then the result for all_answers is NONE. Else the calls to the first argument will have produced SOME lst1, SOME lst2, ... SOME lstn and the result of all_answers is SOME lst where lst is lst1, lst2, ..., lstn appended together (order doesn’t matter). Hints: The sample solution is 8 lines. It uses a helper function with an accumulator and uses @. Note all_answers f [] should evaluate to SOME []. *)

fun all_answers f xs =
	let fun fold (f, acc, xs) =
    case xs of 
			[] => SOME acc
			| x::xs' => case f(x) of
									  NONE => NONE
										| SOME(y) => fold (f, (y @ acc), xs')
	in
		case xs of
		  [] => SOME []
			| _ => fold (f, [], xs)
	end

val all_answers_test_1 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = SOME []
val all_answers_test_2 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val all_answers_test_3 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [1,1,1] = SOME [1,1,1]


(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(* 9. (This problem uses the pattern datatype but is not really about pattern-matching.) A function g has
been provided to you.
(a) Use g to define a function count_wildcards that takes a pattern and returns how many Wildcard patterns it contains. *)

fun count_wildcards p = g (fn _ => 1) (fn _ => 0) p

val count_wildcards_test_1 = count_wildcards Wildcard = 1
val count_wildcards_test_2 = count_wildcards (ConstructorP ("M", Wildcard)) = 1
val count_wildcards_test_3 = count_wildcards (TupleP [Wildcard, UnitP, Wildcard]) = 2


(* (b) Use g to define a function count_wild_and_variable_lengths that takes a pattern and returns the number of Wildcard patterns it contains plus the sum of the string lengths of all the variables in the variable patterns it contains. (Use String.size. We care only about variable names; the constructor names are not relevant.) *)

fun count_wild_and_variable_lengths p = g (fn _ => 1) (fn x => String.size(x)) p

val count_wild_and_variable_lengths_test_1 = count_wild_and_variable_lengths (Variable("a")) = 1
val count_wild_and_variable_lengths_test_2 = count_wild_and_variable_lengths UnitP = 0
val count_wild_and_variable_lengths_test_3 = count_wild_and_variable_lengths Wildcard = 1
val count_wild_and_variable_lengths_test_4 = count_wild_and_variable_lengths (Variable "foo") = 3
val count_wild_and_variable_lengths_test_5 = count_wild_and_variable_lengths (ConstructorP ("M", Wildcard)) = 1
val count_wild_and_variable_lengths_test_6 = count_wild_and_variable_lengths (TupleP [Wildcard, Variable "bar", UnitP, Wildcard]) = 5


(* (c) Use g to define a function count_some_var that takes a string and a pattern (as a pair) and returns the number of times the string appears as a variable in the pattern. We care only about variable names; the constructor names are not relevant. *)

fun count_some_var (s, p) = g (fn _ => 1) (fn x => if s = x then 1 else 0) p

val count_some_var_test_1 = count_some_var ("x", Variable("x")) = 1
val count_some_var_test_2 = count_some_var ("foo", UnitP) = 0
val count_some_var_test_3 = count_some_var ("foo", Wildcard) = 1
val count_some_var_test_4 = count_some_var ("foo", TupleP [Variable "foo", UnitP, Variable "foo", Variable "bar"]) = 2


(* 10. Write a function check_pat that takes a pattern and returns true if and only if all the variables appearing in the pattern are distinct from each other (i.e., use di↵erent strings). The constructor names are not relevant. Hints: The sample solution uses two helper functions. The first takes a pattern and returns a list of all the strings it uses for variables. Using foldl with a function that uses @ is useful in one case. The second takes a list of strings and decides if it has repeats. List.exists may be useful. Sample solution is 15 lines. These are hints: We are not requiring foldl and List.exists here, but they make it easier. *)

fun var_values p =
    case p of
	  Variable x          => [x]
	  | TupleP ps         => List.foldl (fn (p, acc) => (var_values p) @ acc) [] ps
	  | ConstructorP(_,p) => var_values p
	  | _                 => []

val var_values_test_1 = var_values (TupleP [Variable "foo", UnitP, Variable "foo", Variable "bar"]) = ["bar", "foo", "foo"]
val var_values_test_2 = var_values UnitP = []
  
fun has_repeats xs =
  case xs of 
    [] => false
	| x::xs' => if List.exists (fn y => x = y) xs' then true else has_repeats xs'

val has_repeats_test_1 = has_repeats ["x", "y"] = false
val has_repeats_test_2 = has_repeats ["x", "x"] = true

fun check_pat p = not ((has_repeats o var_values) p)

val check_pat_test_1 = check_pat (Variable("x")) = true
val check_pat_test_2 = check_pat Wildcard = true
val check_pat_test_3 = check_pat (TupleP [Variable "foo", UnitP, Variable "foo", Variable "bar"]) = false



(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string
