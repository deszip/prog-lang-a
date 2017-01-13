(* Coursera Programming Languages, Homework 3, Provided Code *)
(*
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

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

*)


(* 1. Write a function only_capitals that takes a string list and returns a string list that has only the strings in the argument that start with an uppercase letter. Assume all strings have at least 1 character. Use List.filter, Char.isUpper, and String.sub to make a 1-2 line solution. *)

val only_capitals = List.filter (fn x=> (Char.isUpper o String.sub) (x, 0))


(* 2. Write a function longest_string1 that takes a string list and returns the longest string in the list. If the list is empty, return "". In the case of a tie, return the string closest to the beginning of the list. Use foldl, String.size, and no recursion (other than the implementation of foldl is recursive). *)

val longest_string1 = List.foldl (fn (x, acc) => if String.size x > String.size acc then x else acc) ""


(* 3. Write a function longest_string2 that is exactly like longest_string1 except in the case of ties it returns the string closest to the end of the list. Your solution should be almost an exact copy of longest_string1. Still use foldl and String.size. *)

val longest_string2 = List.foldl (fn (x, acc) => if String.size x >= String.size acc then x else acc) ""


(* 4 *)

fun longest_string_helper f = List.foldl (fn (x, acc) => if f(String.size x, String.size acc) then x else acc) ""
val longest_string3 = longest_string_helper (fn (a, b) => a > b)
val longest_string4 = longest_string_helper (fn (a, b) => a >= b)


(* 5. Write a function longest_capitalized that takes a string list and returns the longest string in the list that begins with an uppercase letter, or "" if there are no such strings. Assume all strings have at least 1 character. Use a val-binding and the ML library’s o operator for composing functions. Resolve ties like in problem 2. *)

val longest_capitalized =  longest_string1 o only_capitals


(* 6. Write a function rev_string that takes a string and returns the string that is the same characters in reverse order. Use ML’s o operator, the library function rev for reversing lists, and two library functions in the String module. (Browse the module documentation to find the most useful functions.) *)

val rev_string = String.implode o List.rev o String.explode


(* Tests *)

val only_capitals_test_1 = only_capitals ["A","B","C"] = ["A","B","C"]
val only_capitals_test_2 = only_capitals [] = []
val only_capitals_test_3 = only_capitals ["foo", "bar", "baz"] = []
val only_capitals_test_4 = only_capitals ["foo", "bar", "Baz"] = ["Baz"]

val longest_string1_test_1 = longest_string1 ["A","bc","C"] = "bc"
val longest_string1_test_2 = longest_string1 ["ba","bc","C"] = "ba"
val longest_string1_test_3 = longest_string1 [] = ""
val longest_string1_test_4 = longest_string1 ["", "bc"] = "bc"

val longest_string2_test_1 = longest_string2 ["A","bc","C"] = "bc"
val longest_string2_test_2 = longest_string2 ["ba","bc","C"] = "bc"

val longest_string3_test_1 = longest_string1 ["A","bc","C"] = "bc"
val longest_string3_test_2 = longest_string1 ["ba","bc","C"] = "ba"
val longest_string3_test_3 = longest_string1 [] = ""
val longest_string3_test_4 = longest_string1 ["", "bc"] = "bc"

val longest_string4_test_1 = longest_string2 ["A","bc","C"] = "bc"
val longest_string4_test_2 = longest_string2 ["ba","bc","C"] = "bc"

val longest_capitalized_test_1 = longest_capitalized ["A","bc","C"] = "A"
val longest_capitalized_test_2 = longest_capitalized ["a","a","a"] = ""

val rev_string_test_1 = rev_string "abc" = "cba"
val rev_string_test_2 = rev_string "" = ""