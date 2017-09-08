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
Problem found from previous submission:
    * Didn't read the description carefully!
    The rule is if a score of 0 is reached => score is
    the sum of card values.
*)

fun careful_player (cl, goal) =
    let
        fun find_lowest (hcl, ncard, sm_cards) =
            case hcl of
                [] => NONE
                | (hc::hcl') =>
                    if sm_cards - (card_value hc) + (card_value ncard) = goal
                    then SOME ((Discard hc) :: [Draw])
                    else find_lowest (hcl', ncard, sm_cards)

        fun helper (cl, hcl) =
            if (score (hcl, goal)) = 0 then []
            else if (sum_cards hcl) + 10 < goal then
                case cl of
                    [] => [Draw]
                    | c::cl' => Draw :: helper (cl', c::hcl)
            else (* score > 0 and sum_cards + 10 >= goal *)
                case (cl, hcl) of
                    ([], _) => [] (* Discard card increases score *)
                    | (c::cl', _) =>
                        if (card_value c) + sum_cards hcl <= goal
                        then
                            if (score (c::hcl, goal)) < (score (hcl,goal))
                            then Draw :: helper(cl', c::hcl)
                            else []
                        else
                            case find_lowest (hcl, c, sum_cards hcl) of
                                NONE => []
                                | SOME nmove => nmove
    in helper (cl, [])
    end
