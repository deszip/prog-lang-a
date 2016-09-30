(* Helpers *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* Target functions *)

fun all_except_option (str, sl) =
  case sl of
    [] => NONE
    | x::xs => case same_string(str, x) of
                 true => SOME(xs)
                 | false => case all_except_option(str, xs) of
                              NONE => NONE
                              | SOME y => SOME(x::y) 
  

fun get_substitutions1 (subs, s) =
  case subs of
    [] => []
    | (x :: xs) => case all_except_option(s, x) of 
                     NONE => get_substitutions1(xs, s)
                     | SOME(ys) => ys @ get_substitutions1(xs, s) 
                                                    

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

type Name = {first:string, middle:string, last:string}
fun similar_names (subs, name) =
  let fun aux(subs, acc) =
    case subs of
      [] => acc
      | (x::xs) => aux(xs, acc @ [{first=x, middle=(#middle name), last=(#last name)}]) 
  in
    aux(get_substitutions2(subs, #first name), [name])
  end


datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun card_color (suit, rank) = 
  case suit of
    Spades => Black
    | Clubs => Black
    | Diamonds => Red
    | Hearts => Red


fun card_value (suit, rank) =
  case rank of
    Jack => 10
    | Queen => 10
    | King => 10
    | Ace => 11
    | Num i => i


fun remove_card(cs, c, e) =
  case cs of
    [] => raise e
    | x::xs => case c = x of
                 true => xs
                 | false => case remove_card(xs, c, e) of
                              [] => [x]
                              | y::ys => x::y::ys 


fun all_same_color (cs) =
  case cs of
    [] => true
    | a::[] => true
    | a::b::tail => case card_color(a) = card_color(b) of
                    true => all_same_color(b::tail)
                    | false => false


fun sum_cards (cs) =
  let fun aux (cs, acc) =
    case cs of
      [] => acc
      | x::xs => aux(xs, acc + card_value(x))
  in
    aux(cs, 0)
  end


fun score (cs, goal) = 
  case (sum_cards(cs), goal) of
    (sum, goal) => case sum > goal of
                     true => (sum - goal) * 3
                     | false => goal - sum


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