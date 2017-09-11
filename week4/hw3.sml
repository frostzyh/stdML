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

(**** you can put all your code here ****)

fun only_capitals xs = List.filter (fn x => Char.isUpper (String.sub(x,0))) xs

fun longest_string1 xs = case xs of [] => "" |
    _ => List.foldl (fn (x,y) =>
        if (String.size x) > (String.size y) then x else y) "" xs

fun longest_string2 xs = case xs of [] => "" |
    _ => List.foldl (fn (x,y) =>
        if (String.size x) >= (String.size y) then x else y) "" xs

(* longest_string_helper is passed a function that behaves like > *)
fun longest_string_helper f = List.foldl (fn (x,y) => if f(String.size x, String.size y) then x else y) ""


val longest_string3 = longest_string_helper (fn (x,y) => x > y)

val longest_string4 = longest_string_helper (fn (x,y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode

(* Question 7,8 *)


fun first_answer f xs =
	case xs of
		[] => raise NoAnswer
		| x::xs' =>
			case f x of
				SOME y => y
				| NONE => first_answer f xs'

fun all_answers f xs =
	let
		fun helper f acc xs =
			case xs of
				[] => SOME acc
				| x::xs' =>
					case f x of
						NONE => NONE
						| SOME y => helper f (y @ acc) xs'
	in helper f [] xs
	end
(* Question 9 a,b,c *)

fun count_wildcards ptn = g (fn x => 1) (fn y => 0) ptn

fun count_wild_and_variable_lengths ptn = g (fn x => 1) (fn y => String.size y) ptn

fun count_some_var (s, p) = g (fn x => 0) (fn y => if y = s then 1 else 0) p

(* Question 10 *)
fun check_pat ptn =
	let
		fun getStrList ptn =
			case ptn of
				Variable x => [x]
				| TupleP ps => List.foldl (fn (p,acc) => (getStrList p) @ acc) [] ps
				| ConstructorP(_,p) => getStrList p
				| _ => []
		fun checkDup xs =
			case xs of
				[] => true
				| x::xs' => (not(List.exists (fn y => x = y) xs'))  andalso checkDup xs'
	in checkDup (getStrList ptn)
	end

fun match (va,pt) =
	case (va,pt) of
		(_, Wildcard) => SOME [] |
		(Unit, UnitP) => SOME [] |
		(Const x, ConstP y) => if x = y then SOME [] else NONE |
		(Constructor (s1,v), ConstructorP (s2,p)) => if s1=s2 then match (v,p) else NONE |
		(v, Variable s) => SOME [(s,v)] |
		(Tuple vl, TupleP pl) =>
			if List.length vl = List.length pl
			then all_answers match (ListPair.zip(vl,pl))
			else NONE
		| _ => NONE

fun first_match va plst =
	SOME (first_answer (fn pt => match(va,pt)) plst) handle NoAnswer => NONE

(* Challenge Problem *)
(*)
datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string


fun pattern_to_type (lst, pat) =
 case pat of
     UnitP => UnitT
   | ConstP _ => IntT
   | TupleP ps => TupleT (List.map (fn x => pattern_to_type(lst, x)) ps)
   | ConstructorP(str, p) =>
     let fun cons_match x =
             case x of
                 (s, _, pp) => s = str
                               andalso (pattern_to_type(lst, p) = pp orelse
                                        pattern_to_type(lst, p) = Anything)
     in case List.find cons_match lst of
            SOME (_, a, _) => Datatype a
          | NONE => raise NoAnswer
     end
   | _ => Anything

fun get_lenient (t1, t2) =
     if t1 = t2
     then t1
     else case (t1, t2) of
              (_, Anything) => t1
            | (Anything, _) => t2
            | (TupleT ps1, TupleT ps2) =>
              if List.length ps1 = List.length ps2
              then TupleT(List.map get_lenient (ListPair.zip(ps1, ps2)))
              else raise NoAnswer
            | (_, _) => raise NoAnswer

 (* Check the typ of patterns. First find all the typs of given patterns,
 if any of them is NONE return NONE, otherwise get the most lenient typ
 from all the typs. If no such typ, return NONE. *)
fun typecheck_patterns (lst, ps) =
 let val typs = List.map (fn x => pattern_to_type(lst, x)) ps
                handle NoAnswer => []
 in
     case typs of
         [] => NONE
       | head::tail => SOME (List.foldl get_lenient head tail)
                       handle NoAnswer => NONE
 end

 *)
