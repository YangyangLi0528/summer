(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(*ture is some, false is none*)
fun someornone (x,xs) =
    case xs of
	x'::xs' => if same_string(x,x') then true
		   else someornone (x,xs')
      | [] => false

fun listwithoutx (x,xs) =
    case xs of
	[] => []
      | x'::xs' => if same_string(x,x') then listwithoutx (x, xs')
		   else x'::listwithoutx (x,xs')
					 
fun all_except_option (x,xs) =
    if someornone (x,xs) then SOME(listwithoutx(x,xs)) 
    else NONE

fun get_substitutions1 (xs,x) =
    case xs of
	[] => []
      | x'::xs' => case all_except_option (x,x') of
		       NONE => get_substitutions1 (xs',x)
		     | SOME y => y @ get_substitutions1 (xs',x)

fun get_substitutions2 (xs,x) =
    let fun tail (x,xs,acc) =
	    case xs of
		[] => acc
	      | x'::xs' => (case all_except_option (x,x') of
				NONE => tail (x,xs',acc)
			     | SOME y => tail (x,xs',y @ acc))
    in	tail (x,xs,[])    end
	
fun similar_names (xs,x) =
    let fun helper (substitutions,acc) =
	    case substitutions of
		[] => acc
	      | y::ys => case x of
			     {first = a, middle = b, last = c} => helper (ys, {first = y, middle = b, last = c} :: acc)
    in
	let val cantstand = case x of {first=a, middle=b, last=c} => a in
	helper (get_substitutions2 (xs,cantstand),[x]) end end 
		  

			  
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

	      (* put your solutions for problem 2 here *)
fun card_color (x) =
    case x of
	(Clubs,_) => Black
      | (Diamonds,_) => Red
      | (Hearts,_) => Red
      | (Spades,_) => Black

fun card_value (x) =
    case x of
	(_,Num y) => y
      | (_,Ace) => 11
      | _ => 10

fun remove_card (cs,c,e) =
    case cs of
	[] => raise e
      | c'::cs' => if c = c' then cs' else c'::remove_card(cs',c,e)

fun all_same_color (xs) =
    case xs of
	[] => true
      | x'::[] => true
      | x'::x''::xs' => if card_color (x') = card_color (x'') then all_same_color(x''::xs') else false

fun sum_cards (xs) =
    let fun sum (xs, acc) =
	    case xs of
		[] => acc
	      | x'::xs' => sum (xs', acc + card_value(x'))
    in sum (xs, 0) end

fun score (xs,x) =
let fun pscore (xs,x) = 
    if sum_cards (xs) > x then 3 * (sum_cards(xs) - x)
    else x - sum_cards (xs)
in if all_same_color(xs) then pscore(xs,x) div 2
   else pscore(xs,x)
end

fun officiate (cardlist,movelist,goal) =
    let fun currentscore (cardlist1, movelist, holdcard) =
	    if sum_cards (holdcard) > goal then score (holdcard, goal) else
	    case movelist of
		[] => score(holdcard, goal)
	      | x::xs => case x of
			     Discard a => currentscore (cardlist1, xs, remove_card (holdcard, a, IllegalMove))
			  |  Draw => case cardlist1 of
					    [] => score (holdcard, goal)
					  | z::zs => currentscore (zs,xs,z::holdcard)
    in currentscore(cardlist,movelist,[]) end
	

val test1 = all_except_option ("string", ["string"]);
val test1a = all_except_option("strings", ["strings", "are", "fun"]);
val test1b = all_except_option("strings", ["strings", "strings", "fun"]);
val test2 = get_substitutions1 ([["foo"],["there"]], "foo");
val test2a = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred");
val test2b = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff");
val test3 = get_substitutions2 ([["foo"],["there"]], "foo");
val test3a = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred");
val test3b = get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff");
val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"});
val test5 = card_color (Clubs, Num 2);
val test6 = card_value (Clubs, Num 2);
val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove);
val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)];
val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)];
val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10);
val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15);
val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],[Draw,Draw,Draw,Draw,Draw],42);