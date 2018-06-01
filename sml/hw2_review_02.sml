(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(x, y) =
  let fun aux (z) =
        case z of
          [] => []
        | head::tail => if same_string(x, head)
                        then aux(tail)
                        else head :: aux(tail)
      val result = aux(y)
  in
      if result = y
      then NONE
      else SOME result
  end

fun get_substitutions1 ([], s) = []
  | get_substitutions1 (x::xs, s) =
    case all_except_option (s, x) of
      NONE   => get_substitutions1 (xs, s)
    | SOME y => y @ get_substitutions1 (xs, s)


fun get_substitutions2 (h, s) =
  let fun helper ([], acc)    = acc
        | helper (x::xs, acc) = 
            case all_except_option (s, x) of
              NONE   => helper (xs, acc)
            | SOME y => helper (xs, acc @ y)
  in
    helper(h, [])
  end

fun similar_names (subs, {first=f, middle=m, last=l}) =
  let fun substitute([]) = []
        | substitute(x::xs) = {first=x, middle=m, last=l} :: substitute(xs)
  in 
    {first=f, middle=m, last=l} :: substitute(get_substitutions2(subs, f))
  end
  



(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color (Spades, _) = Black
  | card_color (Clubs, _ ) = Black
  | card_color (_,_)       = Red

fun card_value (_, Num i) = i
  | card_value (_, Ace)   = 11
  | card_value(_, _)      = 10

fun remove_card (cs, c, e) =
  let fun aux cs =
    case cs of
      [] => raise e
    | head::tail => if head = c                                              
                    then tail                                                       
                    else head :: aux(tail)
  in
    aux cs
  end

fun all_same_color cs =
  case cs of
       []      => true
     | _::[]   => true
     | x::(y::z) => if card_color x = card_color y
                  then all_same_color (y::z)
                  else false

(* non-tail recursive
fun sum_cards cs =
  case cs of
       []     => 0
      | x::xs  => card_value x + sum_cards xs
*)

fun sum_cards cs =
  let fun aux (cards, acc) =
    case cards of
      []    => acc
    | x::xs => aux(xs, acc + (card_value x))
  in
    aux(cs,0)
  end

fun score (cs, goal) =
  let
    val sum = sum_cards cs
    val prelim = if sum > goal
                 then 3 * (sum - goal)
                 else goal - sum
  in
    if all_same_color cs
    then prelim div 2
    else prelim
  end

fun officiate (cards, moves, goal) =
  let
    fun helper (cards, moves, held_cards) =
      case moves of
        []              => score(held_cards, goal)
      | Draw::ms        => (case cards of
                                 [] => score (held_cards, goal)
                            | c::cs => if sum_cards(c::held_cards) > goal
                                      then score(c::held_cards, goal)
                                      else helper(cs, ms, c::held_cards))
      | (Discard c)::ms => helper(cards, ms, remove_card 
                                             (held_cards, c,IllegalMove))
  in
    helper (cards, moves, [])
  end