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

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
fun only_capitals list =
    List.filter (fn str => Char.isUpper(String.sub(str, 0))) list

fun longest_string1 list =
    List.foldl (fn (str,acc) => if String.size str > String.size acc then str else acc) "" list

fun longest_string2 list =
    List.foldl (fn (str,acc) => if String.size str >= String.size acc then str else acc) "" list

fun longest_string_helper f list =
    List.foldl (fn (str,acc) => if f (String.size(str),String.size(acc)) then str else acc) "" list

val longest_string3 = longest_string_helper (fn (x,y) => x > y);

val longest_string4 = longest_string_helper (fn (x,y) => x >= y);

val longest_capitalized = longest_string1 o only_capitals;

val rev_string = String.implode o List.rev o String.explode;

fun first_answer f list =
    case list of
	x::xs => (case f x of
		     SOME v => v
		   | NONE => first_answer f xs)
	| [] => raise NoAnswer;

fun all_answers f list =
    let fun accumulate (lst, acc) =
	    case lst of
		x::xs => (case f x of
			     NONE => NONE
			  | SOME v => accumulate(xs, v@acc))
	     | [] => SOME acc
    in
	accumulate(list, [])
    end
	
fun count_wildcards p =
    g (fn _ => 1) (fn _ => 0) p

fun count_wild_and_variable_lengths p =
    g (fn _ => 1) (fn s => String.size s) p

fun count_some_var (str, p) =
    g (fn _ => 0) (fn x => if str = x then 1 else 0) p

fun check_pat p =
    let fun var_names (pt, acc) =
	    case pt of
		Variable x => x::acc
	     |  TupleP ps => (case ps of
				  [] => acc
			        | pt' => List.foldl (fn (pt', lst) => var_names(pt',lst)) acc pt')
	     |  ConstructorP(s,pt') => var_names(pt', acc)
	     | _ => acc
	fun name_repeats list =
	    case list of
		l::l' => if List.exists (fn s => s = l) l' then false else name_repeats l'
	      | _ => true
    in
	name_repeats(var_names(p, []))
    end
	
fun match (v, p) =
    case (v, p) of
	(_, Wildcard) => SOME []
      | (x,Variable(s)) => SOME [(s,v)]
      | (Unit, UnitP) => SOME []
      | (Const v1, ConstP v2) => if v1 = v2 then SOME [] else NONE
      | (Tuple vs, TupleP ps) => let val zipped = ListPair.zip (vs, ps) in
				     if List.length vs = List.length ps
				     then all_answers match zipped
				     else NONE (* Submitted as SOME [] which is wrong *)
				 end
      | (Constructor(sv,pv),ConstructorP(sp,pp)) => if sv = sp then match(pv,pp) else NONE
      | _ => NONE

fun first_match v ps =
    SOME (first_answer match (List.map (fn x => (v, x)) ps))
		 handle NoAnswer => NONE