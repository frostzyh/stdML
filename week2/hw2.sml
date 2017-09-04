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
fun sum_cards2 cl =
    let fun helper (cl, acc) =
            case cl of
                [] => acc
                | (_, Ace)::cl' => helper(cl', acc + 1)
                | c::cl' => helper(cl', acc + card_value c)
    in helper(cl, 0) end


fun score_challenge (cl, goal) =
    let
        val sum = sum_cards2 cl
        val p_sum = if sum > goal then 3* (sum - goal) else goal - sum
    in
        p_sum div (if all_same_color cl then 2 else 1)
    end



fun officiate_challenge (cl, ml, goal) =
    let
        fun helper (cl, ml, hc) = (* Card list, move list, held-card list *)
            case ml of
                [] => score_challenge (hc, goal) (* No more moves *)
                | (Discard dc)::ml' => helper(cl, ml', remove_card (hc, dc, IllegalMove))
                | Draw::ml' =>
                    case cl of
                        [] => score_challenge (hc, goal)
                        | c::cl' =>
                            if sum_cards2 (c::hc) > goal
                            then score_challenge (c::hc, goal)
                            else helper (cl', ml', c::hc)
    in helper (cl,ml,[])
    end

(* Rules:
    1. If No card on deck, Draw to finish
    2. Draw when sum_cards(holding) + 10 < goal
    3. If no cards is holding, end.

Pseudocode
Initial : holding one card
if (deck is empty)
    discard all cards on hand, draw, then end.
else
    discard all cards on hand except the smallest one.
    if (card_on_hand + card_on_deck <= goal)
        keep the smaller one on hand
    else
        Discard card_on_hand, then end.
*)
fun careful_player ([], goal) = [Draw]
| careful_player (c::cl, goal) =
    let  (* card-list, goal, moves, hold-cards*)
        fun helper (cl, hcs) =
            case (cl, hcs) of
                ([], hc::[]) => (Discard hc) :: Draw :: [] (* Draw even no card *)
                | ([], hc::hcs') => (Discard hc) :: helper(cl, hcs')
                | (_, hc::hc'::hcs) => (* only keep smallest on hand*)
                    if card_value hc >= card_value hc'
                    then (Discard hc) :: helper(cl, hc'::hcs)
                    else (Discard hc') :: helper(cl, hc::hcs)
                | (c::cl', hc::hcs) =>
                    if (card_value c + card_value hc <= goal)
                    then
                        if card_value c < card_value hc
                        then Draw :: Discard (hc) :: helper (cl', c::hcs)
                        else Draw :: Discard (c) :: helper (cl', hc::hcs)
                    else
                        (Discard hc) :: []
    in Draw :: helper (cl, [c])
    end
