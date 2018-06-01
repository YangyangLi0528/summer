(* Michel Salim, Coursera PL, HW2 *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string (s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* part a *)
fun all_except_option (s: string, lst: string list) =
    case lst of
        [] => NONE
      | s1::lst1 => if same_string(s, s1)
                    then SOME lst1
                    else case all_except_option(s, lst1) of
                             NONE => NONE
                           | SOME lst2 => SOME (s1::lst2)

(* part b *)
fun get_substitutions1 (subs: string list list, s: string) =
    case subs of
        [] => []
      | sub1::subs1 => let val tl_ans = get_substitutions1(subs1, s)
                       in case all_except_option(s, sub1) of
                              NONE => tl_ans
                            | SOME hd_ans => hd_ans @ tl_ans
                       end

(* part c *)
fun get_substitutions2 (subs: string list list, s: string) =
    let fun subs_into (subs: string list list, outls: string list) =
            case subs of
                [] => outls
              | sub1::subs1 =>
                subs_into (
                    subs1,
                    case all_except_option(s, sub1) of
                        NONE => outls
                      | SOME hd_ans => hd_ans @ outls
                )
    in
        subs_into (subs, [])
    end

(* part d *)
fun similar_names (subs: string list list,
                   full_name: {first: string, middle: string, last: string}) =
    let val {first=f_name, middle=m_name, last=l_name} = full_name
        fun complete_names(first_names: string list) =
            case first_names of
                [] => []
              | s::lst => {first=s, middle=m_name, last=l_name} ::
                          complete_names(lst)
    in
        complete_names(f_name::get_substitutions1(subs, f_name))
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
(* part a *)
fun card_color (c: card) =
    case c of
        (Clubs, _) => Black
      | (Diamonds, _) => Red
      | (Hearts, _) => Red
      | (Spades, _) => Black

(* part b *)
fun card_value (c: card) =
    case c of
        (_, Num x) => x
      | (_, Ace) => 11
      | _ => 10

(* part c *)
fun remove_card (cs: card list, c: card, e: exn) =
    case cs of
        [] => raise e
      | c1::cs1 => if c = c1
                   then cs1
                   else remove_card(cs1, c, e)

(* part d *)
fun all_same_color (cs: card list) =
    case cs of
        [] => true
      | c1::[] => true
      | c1::c2::cs => (card_color(c1) = card_color(c2)) andalso
                      all_same_color(c2::cs)

(* part e *)
fun sum_cards (cs: card list) =
    let fun summer (cs: card list, tot: int) =
            case cs of
                [] => tot
              | c::cs1 => summer(cs1, tot+card_value(c))
    in
        summer(cs, 0)
    end

(* part f *)
fun score (cs: card list, goal: int) =
    let val sum = sum_cards cs
        val prelim_score = if sum > goal
                           then 3 * (sum - goal)
                           else (goal - sum)
    in if all_same_color cs
       then prelim_score div 2
       else prelim_score
    end

(* part g *)
fun officiate (cs: card list, ms: move list, goal: int) =
    let fun game_state (held_cs: card list, cs: card list, ms: move list) =
            case ms of
                [] => score (held_cs, goal)
              | (Discard c)::ms1 =>
                game_state (remove_card (held_cs, c, IllegalMove),
                            cs,
                            ms1)
              | Draw::ms1 =>
                case cs of
                    [] => score (held_cs, goal)
                  | c::cs1 =>
                    let val held_cs1 = c::held_cs
                        val sum = sum_cards held_cs1
                    in
                        if sum > goal
                        then score (held_cs1, goal)
                        else game_state (held_cs1, cs1, ms1)
                    end
    in game_state ([], cs, ms)
    end

(* Q3 - Challenge Problems *)
(* part a *)
fun count_aces (cs: card list) =
    case cs of
        [] => 0
      | (_, Ace)::cs1 => 1 + count_aces cs1
      | _::cs1 => count_aces cs1

fun min (ns: int list) =
    let fun min2 (x, y) = if x < y then x else y
    in case ns of
           n::ns => foldr min2 n ns
    end

fun sums_of_cards (cs: card list) =
    let val ace_count = count_aces cs
        val sum = sum_cards cs
        fun gen_sums (ace_as_1 : int) =
            case ace_as_1 of
                0 => [sum]
             |  n => (sum - 10*n) :: gen_sums (n-1)
    in gen_sums ace_count
    end

fun score_challenge (cs: card list, goal: int) =
    let val all_same_ans = all_same_color cs
        fun score_with_sum (sum: int) =
            let val prelim_score = if sum > goal
                                   then 3 * (sum - goal)
                                   else (goal - sum)
            in if all_same_ans then prelim_score div 2 else prelim_score
            end
    in min (map score_with_sum (sums_of_cards cs))
    end

fun officiate_challenge (cs: card list, ms: move list, goal: int) =
    let fun game_state (held_cs: card list, cs: card list, ms: move list) =
            case ms of
                [] => score_challenge (held_cs, goal)
              | (Discard c)::ms1 =>
                game_state (remove_card (held_cs, c, IllegalMove),
                            cs,
                            ms1)
              | Draw::ms1 =>
                case cs of
                    [] => score_challenge (held_cs, goal)
                  | c::cs1 =>
                    let val held_cs1 = c::held_cs
                        val sum = min (sums_of_cards held_cs1)
                    in
                        if sum > goal
                        then score_challenge (held_cs1, goal)
                        else game_state (held_cs1, cs1, ms1)
                    end
    in game_state ([], cs, ms)
    end

(* part b *)
fun careful_player (cs: card list, goal: int) =
    let fun game_state (held_cs: card list, sum_needed: int, cs: card list) =
            case sum_needed of
                0 => []
              | _ =>
                case cs of
                    [] => if sum_needed > 10
                          then [Draw]
                          else []
                  | c::cs1 =>
                    let val next_val = card_value c
                    in
                        if sum_needed > 10
                           orelse next_val <= sum_needed
                        then Draw :: game_state (
                                 c::held_cs,
                                 sum_needed - next_val,
                                 cs1)
                        else case held_cs of
                                 [] => []
                               | h::held_cs1 =>
                                 if (next_val - card_value(h)) = sum_needed
                                 then [Discard(h), Draw]
                                 else []
                    end
    in game_state ([], goal, cs)
    end