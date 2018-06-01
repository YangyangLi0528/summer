(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = 
           Wildcard
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

infix |>
fun x |> f = f x

(* Problem 1. *)
fun only_capitals strs =
    List.filter (fn s => (s,0) |> String.sub |> Char.isUpper) strs 

(* Problem 2 *)
fun longest_string1 strs =
    List.foldl (fn (x,y) => (if String.size(x) > String.size(y) then x else y)) ("") (strs) 

(* Problem 3 *)
fun longest_string2 strs =
    List.foldl (fn (x,y) => (if String.size(x) >= String.size(y) then x else y)) ("") (strs) 

(* Problem 4 *)
fun longest_string_helper f strs =
    List.foldl (fn (x,y) => if f(String.size(x),String.size(y)) then x else y) ("") (strs)

val longest_string3 = longest_string_helper (fn (x,y) => x > y)
val longest_string4 = longest_string_helper (fn (x,y) => x >= y)

(* Problem 5 *)
val longest_capitalized = longest_string1 o only_capitals

(* Problem 6 *)
val rev_string = String.implode o List.rev o String.explode

(* Problem 7 *) 
fun first_answer f xs =
    case xs of
        [] => raise NoAnswer
      | x::xs' => case f(x) of
                   SOME(y) => y
                 | NONE => (first_answer f xs')

(* Problem 8 *)
fun all_answers f xs =
        let
            fun helper(f,xs,acc) =
                case xs of
                    [] => SOME acc
                  | x::xs' => case f(x) of
                                NONE => NONE
                              | SOME(y) => helper(f, xs', y @ acc)
        in
            helper(f,xs,[])
        end

(* Problem 9 *)
fun count_wildcards pat = g (fn () => 1) (fn str => 0) (pat)

fun count_wild_and_variable_lengths pat =
    g (fn () => 1) (fn str => String.size(str)) (pat)

fun count_some_var (str,pat) =
    g (fn () => 1) (fn x => if x = str then 1 else 0) (pat)

(* Problem 10 *)
fun check_pat pat =
    let
        fun all_strings pat =
            (* This helper function returns all the strings found in Variables in pat *)
            case pat of
                Variable str => [str]
                (* List.map returns a nested list [[str1],[str2],..[strn]] in this case.
                I'm using List.concat to "flatten" it to [str1,str2,strn] *)
              | TupleP ps => List.concat((List.map (all_strings) (ps)))
              | ConstructorP(_,p) => all_strings(p)
              | _ => []

        fun all_str_different(strs) =
            (* This helper function returns true if all strings in the list strs are unique*)
            case strs of
                [] => true
                (* Every call to List.exists returns false if the head (s) isn't in the tail (strs')
                I'm therefore using 'not' on the result to get true instead. If we get true every
                time, the final result is true because of the 'andalso'. *)
              | s::strs' => not(List.exists (fn x => x = s) (strs')) andalso all_str_different(strs')
    in
        pat |> all_strings |> all_str_different
    end

(* Problem 11 *)
fun match (v,p) =
    case (v,p) of
        (_,Wildcard) => SOME []
      | (_,Variable(s)) => SOME [(s,v)]
      | (Unit,UnitP) => SOME []
      | (Const(n),ConstP(i)) => if i = n
                                then SOME []
                                else NONE 
      | (Tuple(vs),TupleP(ps)) => if length(ps) = length(vs)
                                 then all_answers match (ListPair.zip(vs,ps))
                                 else NONE
      | (Constructor(s1,va),ConstructorP(s2,pat)) => if s1 = s2 andalso isSome(match(va,pat))
                                                    then match(va,pat)
                                                    else NONE
      | _ => NONE

(* Problem 12 *)
fun first_match v pats =
    SOME (first_answer match (map (fn p => (v,p)) pats)) (* Map creates list [(v,p1),(v,p2) ... (v,pn)] *)
    handle NoAnswer => NONE