(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* fn : string * string list -> string list option *)
fun all_except_option (str, sl) =
    let fun helper (str,sl) =
        case sl of
            [] => []
            | s::sl' => if same_string (s,str) then sl' else s::helper(str,sl')
        val ans = helper(str, sl)
    in
        if List.length ans = List.length sl
        then NONE
        else SOME ans
    end

(* fn : string list list * string -> string list *)
fun get_substitutions1 (sll, str) =
    case sll of
        [] => []
        | sl::sll' =>
            case all_except_option(str, sl) of
                NONE => get_substitutions1(sll', str)
                | SOME li => li @ get_substitutions1 (sll', str)

(* fn : string list list * string -> string list *)
fun get_substitutions2 (sll, str) =
    let fun helper (sll, str, acc) =
            case sll of
                [] => acc
                | sl::sll' =>
                    case all_except_option(str, sl) of
                        NONE => helper (sll', str, acc)
                        | SOME li => helper (sll', str, acc @ li)
    in
        helper (sll, str, [])
    end


fun similar_names (sll, {first:string,middle:string,last:string}) =
    let
        val name_list = get_substitutions1(sll, first)
        fun helper [] = []
        | helper ( n::nl' ) =
            {first = n, middle = middle, last = last} :: helper(nl')
    in {first = first, middle = middle, last = last} :: helper (name_list)
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
