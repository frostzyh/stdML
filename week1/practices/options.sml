(* fn : int list -> int option *)
fun max (xs : int list) =
    if null xs
    then NONE
    else
        let
            val tail_ans = max(tl xs)
        in
            if isSome tail_ans andalso valOf tail_ans > hd xs
            then tail_ans
            else SOME (hd xs)
        end
        
(*Improved max function*)
fun max2 (xs : int list) =
    if null xs
    then NONE
    else
        let 
            fun max_nonempty (xs : int list) = 
                if null (tl xs)
                then hd xs
                else
                    let 
                        val tail_ans = max_nonempty(tl xs)
                    in
                        if hd xs >tail_ans
                        then hd xs
                        else tail_ans
                    end
        in
            SOME (max_nonempty xs)
        end
        
