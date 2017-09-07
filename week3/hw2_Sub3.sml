(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* fn : string * string list -> string list option *)
fun all_except_option (str, sl) =
    case sl of
        [] => NONE
        | s::sl' =>
            if same_string(s,str)
            then SOME sl'
            else case all_except_option (str, sl') of
                    NONE => NONE
                    | SOME p => SOME (s::p)

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
    let fun helper (sll, acc) =
            case sll of
                [] => acc
                | sl::sll' =>
                    case all_except_option(str, sl) of
                        NONE => helper (sll', acc)
                        | SOME li => helper (sll', acc @ li)
    in
        helper (sll, [])
    end


fun similar_names (sll, name) =
    let
        val {first = first, middle = middle, last = last} = name
        val name_list = get_substitutions1(sll, first)
        fun helper [] = []
        | helper ( n::nl' ) =
            {first = n, middle = middle, last = last} :: helper(nl')
    in name :: helper (name_list)
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
fun card_color (s,_ : rank) =
    case s of
        Diamonds  => Red
        | Hearts => Red
        | _ => Black


fun card_value (s:suit, r) =
    case r of
        Ace => 11
        | Num n => n
        | _ => 10

fun remove_card (cs : card list, c : card, e) =
    case cs of
        [] => raise e
        | cc::cs' => if cc = c then cs' else cc :: remove_card (cs',c,e)

fun all_same_color cl =
    case cl of
        front::middle::tail => card_color front = card_color middle andalso
            all_same_color (middle::tail)
        | _ => true

fun sum_cards cl =
    let
        fun helper (cl, acc) =
            case cl of
                [] => acc
                | c::cl' => helper(cl', acc + card_value c)
    in helper(cl, 0) end


fun score (cl, goal) =
    let
        val sum = sum_cards cl
        val p_sum = if sum > goal then 3* (sum - goal) else goal - sum
    in
        p_sum div (if all_same_color cl then 2 else 1)
    end

(* Hint: keep moving even card list is empty. (next action may be Discard)*)
fun officiate (cl, ml, goal) =
    let
        fun helper (cl, ml, hc) = (* Card list, move list, held-card list *)
            case ml of
                [] => score (hc, goal) (* No more moves *)
                | (Discard dc)::ml' => helper(cl, ml', remove_card (hc, dc, IllegalMove))
                | Draw::ml' =>
                    case cl of
                        [] => score (hc, goal)
                        | c::cl' =>
                            if sum_cards (c::hc) > goal
                            then score (c::hc, goal)
                            else helper (cl', ml', c::hc)
    in helper (cl,ml,[])
    end


(*Challenge Problems starts here *)
fun replace_one_Ace (cl) =
    case cl of
        [] => []
        | (st,Ace)::cl' => (st, Num 1) :: cl'
        | c::cl' => c :: replace_one_Ace(cl')

fun find_smallest li =
    case li of
        [] => 0
        | e::[] => e
        | e::li' => let val potential = find_smallest li'
                    in if e < potential then e else potential
                    end

fun score_challenge (cl, goal) =
    let
        fun helper (cl) =
            let
                val cl_mod = replace_one_Ace (cl)
            in
                if cl = cl_mod then [score(cl, goal)] else score(cl,goal)::helper(cl_mod)
            end
    in find_smallest(helper cl)
    end

fun replace_one_Ace_move mv =
    case mv of
        [] => []
        | Discard (st, Ace) :: mv' => Discard (st, Num 1) :: mv'
        | m1::mv' => m1::replace_one_Ace_move(mv')

(* Hint: keep moving even card list is empty. (next action may be Discard)*)
fun officiate_challenge (cl, ml, goal) =
    let
        fun helper (cl, ml) = (* Card list, move list *)
            let val cl_mod = replace_one_Ace cl
            in
                if cl = cl_mod
                then [officiate(cl,ml,goal)]
                else  officiate(cl,ml,goal) :: helper(cl_mod, replace_one_Ace_move ml)
            end
    in find_smallest (helper (cl,ml))
    end


(* Rules:
    1. If No card on deck, Draw to finish
    2. Draw when sum_cards(holding) + 10 < goal
    3. If no cards is holding, end.
    4. If it is possible to reach a score of 0 by discarding a card
    followed by drawing a card, then this must be done.
    5. if only one card left on hand and you dont need to draw, then
    discard the card and end.

Pseudocode
if (No need to draw)
    if (no card on hand) => end
    if (only one card on hand) => Discard, end
    if (two or more card on hand) => Discard one, back to function
else (have to draw)
    if (deck is empty) => Draw, end.
    else => Draw, back to function.
*)

fun careful_player ([], goal) = [Draw]
| careful_player (cl, goal) =
    let
        fun helper (cl, hcl) =
            if sum_cards hcl + 10 >= goal (* if no need to draw*)
            then
                case (cl, hcl) of
                    ([],[]) => [Draw] (* rule #4 *)
                    | (c::cl', []) => (* rule #4 *)
                        if card_value c <= goal
                        then [Draw,(Discard c)]
                        else []
                    | (_, hc::[]) => [Discard hc] (* rule #5 *)
                    | (_, hc::hcl') => (Discard hc) :: helper(cl,hcl')
            else
                case cl of
                    [] => [Draw]
                    | c::cl' => Draw :: helper(cl', c::hcl)

    in helper (cl,[])
    end
