(* Helpers *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* Target functions *)

(* a. Write a function all_except_option, which takes a string and a string list. Return NONE if the string is not in the list, else return SOME lst where lst is identical to the argument list except the string is not in it. You may assume the string is in the list at most once. Use same_string, provided to you, to compare strings. Sample solution is around 8 lines. *)
fun all_except_option (str, sl) =
  case sl of
    [] => NONE
    | x::xs => case same_string(str, x) of
                 true => SOME(xs)
                 | false => case all_except_option(str, xs) of
                              NONE => NONE
                              | SOME y => SOME(x::y) 
  

val all_except_option_test_1 = all_except_option ("string", ["string"]) = SOME []
val all_except_option_test_2 = all_except_option ("string", ["foo"]) = NONE
val all_except_option_test_3 = all_except_option ("foo", ["foo", "bar", "baz"]) = SOME ["bar", "baz"]
val all_except_option_test_4 = all_except_option ("bar", ["foo", "bar", "baz"]) = SOME ["foo", "baz"]
val all_except_option_test_5 = all_except_option ("bar", ["foo", "baz", "bar"]) = SOME ["foo", "baz"]

(* b. Write a function get_substitutions1, which takes a string list list (a list of list of strings, the substitutions) and a string s and returns a string list. The result has all the strings that are in some list in substitutions that also has s, but s itself should not be in the result. Example:
     get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
                        "Fred")
     (* answer: ["Fredrick","Freddie","F"] *)
Assume each list in substitutions has no repeats. The result will have repeats if s and another string are both in more than one list in substitutions. Example:
     get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],
                        "Jeff")
(* answer: ["Jeffrey","Geoff","Jeffrey"] *)
Use part (a) and ML’s list-append (@) but no other helper functions. Sample solution is around 6 lines. *)
fun get_substitutions1 (subs, s) =
  case subs of
    [] => []
    | (x :: xs) => case all_except_option(s, x) of 
                     NONE => get_substitutions1(xs, s)
                     | SOME(ys) => ys @ get_substitutions1(xs, s) 
                                                    

val get_substitutions1_test_1 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val get_substitutions1_test_2 = get_substitutions1 ([["Fred","Fredrick"],["Freddie","Fred","F"]], "Fred") = ["Fredrick", "Freddie", "F"]
val get_substitutions1_test_3 = get_substitutions1 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]
val get_substitutions1_test_4 = get_substitutions1 ([["Fred","Fredrick"],["Elizabeth","Betty","Fred"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Elizabeth","Betty","Freddie","F"]

(* c. Write a function get_substitutions2, which is like get_substitutions1 except it uses a tail-recursive local helper function.*)
fun get_substitutions2 (subs, s) =
  let fun aux(subs, s, acc) =
    case subs of
      [] => acc
      | (x :: xs) => case all_except_option(s, x) of 
                       NONE => aux(xs, s, acc)
                       | SOME(ys) =>  aux(xs, s, acc @ ys)
  in
    aux(subs, s, [])
  end

val get_substitutions2_test_1 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val get_substitutions2_test_2 = get_substitutions2 ([["Fred","Fredrick"],["Freddie","Fred","F"]], "Fred") = ["Fredrick", "Freddie", "F"]
val get_substitutions2_test_3 = get_substitutions2 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]
val get_substitutions2_test_4 = get_substitutions2 ([["Fred","Fredrick"],["Elizabeth","Betty","Fred"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Elizabeth","Betty","Freddie","F"]

(* d. Write a function similar_names, which takes a string list list of substitutions (as in parts (b) and (c)) and a full name of type {first:string,middle:string,last:string} and returns a list of full names (type {first:string,middle:string,last:string} list). The result is all the full names you can produce by substituting for the first name (and only the first name) using substitutions and parts (b) or (c). The answer should begin with the original name (then have 0 or more other names). Example:
     similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
                   {first="Fred", middle="W", last="Smith"})
     (* answer: [{first="Fred", last="Smith", middle="W"},
                 {first="Fredrick", last="Smith", middle="W"},
                 {first="Freddie", last="Smith", middle="W"},
                 {first="F", last="Smith", middle="W"}] *)
Do not eliminate duplicates from the answer. Hint: Use a local helper function. Sample solution is around 10 lines. *)
type Name = {first:string, middle:string, last:string}
fun similar_names (subs, name) =
  let fun aux(subs, acc) =
    case subs of
      [] => acc
      | (x::xs) => aux(xs, acc @ [{first=x, middle=(#middle name), last=(#last name)}]) 
  in
    aux(get_substitutions2(subs, #first name), [name])
  end

val similar_names_test_1 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"},
         {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"},
         {first="F", last="Smith", middle="W"}]

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* (a) Write a function card_color, which takes a card and returns its color (spades and clubs are black, diamonds and hearts are red). Note: One case-expression is enough. *)
fun card_color (suit, rank) = 
  case suit of
    Spades => Black
    | Clubs => Black
    | Diamonds => Red
    | Hearts => Red

val card_color_test_1 = card_color (Clubs, Num 2) = Black
val card_color_test_2 = card_color (Spades, Num 2) = Black
val card_color_test_3 = card_color (Diamonds, Num 2) = Red
val card_color_test_4 = card_color (Hearts, Num 2) = Red

(* (b) Write a function card_value, which takes a card and returns its value (numbered cards have their number as the value, aces are 11, everything else is 10). Note: One case-expression is enough. *)
fun card_value (suit, rank) =
  case rank of
    Jack => 10
    | Queen => 10
    | King => 10
    | Ace => 11
    | Num i => i

val card_value_test_1 = card_value (Clubs, Num 2) = 2
val card_value_test_2 = card_value (Clubs, Ace) = 11
val card_value_test_3 = card_value (Clubs, King) = 10


(* (c) Write a function remove_card, which takes a list of cards cs, a card c, and an exception e. It returns a list that has all the elements of cs except c. If c is in the list more than once, remove only the first one. If c is not in the list, raise the exception e. You can compare cards with =. *)
fun remove_card(cs, c, e) =
  case cs of
    [] => raise e
    | x::xs => case c = x of
                 true => xs
                 | false => case remove_card(xs, c, e) of
                              [] => [x]
                              | y::ys => x::y::ys 

val remove_card_test_1 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val remove_card_test_2 = remove_card ([(Hearts, Ace), (Diamonds, Num 3)], (Hearts, Ace), IllegalMove) = [(Diamonds, Num 3)]
val remove_card_test_3 = remove_card ([(Diamonds, Num 3), (Hearts, Ace)], (Hearts, Ace), IllegalMove) = [(Diamonds, Num 3)]
val remove_card_test_4 = ((remove_card ([(Hearts, Num 2)], (Hearts, Ace), IllegalMove); false) handle IllegalMove => true)

(* (d) Write a function all_same_color, which takes a list of cards and returns true if all the cards in the list are the same color. Hint: An elegant solution is very similar to one of the functions using nested pattern-matching in the lectures. *)
fun all_same_color (cs) =
  case cs of
    [] => true
    | a::[] => true
    | a::b::tail => case card_color(a) = card_color(b) of
                    true => all_same_color(b::tail)
                    | false => false

val all_same_color_test_1 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val all_same_color_test_2 = all_same_color [(Hearts, Ace), (Hearts, Ace), (Clubs, Ace)] = false
val all_same_color_test_3 = all_same_color [] = true

(* (e) Write a function sum_cards, which takes a list of cards and returns the sum of their values. Use a locally defined helper function that is tail recursive. (Take “calls use a constant amount of stack space” as a requirement for this problem.) *)
fun sum_cards (cs) =
  let fun aux (cs, acc) =
    case cs of
      [] => acc
      | x::xs => aux(xs, acc + card_value(x))
  in
    aux(cs, 0)
  end

val sum_cards_test_1 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val sum_cards_test_2 = sum_cards [(Clubs, Ace),(Clubs, Num 2)] = 13
val sum_cards_test_3 = sum_cards [] = 0
val sum_cards_test_4 = sum_cards [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)]

(* (f) Write a function score, which takes a card list (the held-cards) and an int (the goal) and computes the score as described above. Scoring works as follows: Let sum be the sum of the values of the held-cards. If sum is greater than goal, the preliminary score is three times (sum−goal), else the preliminary score is (goal − sum) *)
fun score (cs, goal) = 
  case (sum_cards(cs), goal) of
    (sum, goal) => case sum > goal of
                     true => (sum - goal) * 3
                     | false => goal - sum

val score_test_1 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val score_test_2 = score ([(Hearts, Num 2),(Clubs, Num 4)],2) = 12
val score_test_3 = score ([(Hearts, Num 2),(Clubs, Num 4)],6) = 0
val score_test_4 = score ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],42)

(* (g) Write a function officiate, which “runs a game.” It takes a card list (the card-list) a move list (what the player “does” at each point), and an int (the goal) and returns the score at the end of the game after processing (some or all of) the moves in the move list in order. Use a locally defined recursive helper function that takes several arguments that together represent the current state of the game. As described above:
• The game starts with the held-cards being the empty list.
• The game ends if there are no more moves. (The player chose to stop since the move list is empty.)
• If the player discards some card c, play continues (i.e., make a recursive call) with the held-cards not having c and the card-list unchanged. If c is not in the held-cards, raise the IllegalMove exception.
• If the player draws and the card-list is (already) empty, the game is over. Else if drawing causes the sum of the held-cards to exceed the goal, the game is over (after drawing). Else play continues with a larger held-cards and a smaller card-list.
Sample solution for (g) is under 20 lines. *)
fun officiate (cs, ms, goal) =
  let fun process_moves(cs, ms, held) =
      case ms of
        [] => held
        | m::ms_tail => case m of
                          Discard card => process_moves(cs, ms_tail, remove_card(held, card, IllegalMove))
                          | Draw => case cs of
                                      [] => held
                                      | c::_ => case sum_cards(c::held) > goal of
                                                  true => process_moves(remove_card(cs, c, IllegalMove), ms_tail, c::held)
                                                  | false => process_moves(remove_card(cs, c, IllegalMove), ms_tail, c::held)
                                                       
                                                       
  in
    case all_same_color(process_moves(cs, ms, [])) of
      true => score(process_moves(cs, ms, []), goal) div 2
      | false => score(process_moves(cs, ms, []), goal) 
  end

val officiate_test_1 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6
val officiate_test_2 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)], [Draw, Draw, Draw, Draw, Draw], 42) = 3
val officiate_test_3 = ((officiate([(Clubs,Jack),(Spades,Num(8))], [Draw,Discard(Hearts,Jack)], 42); false) handle IllegalMove => true)