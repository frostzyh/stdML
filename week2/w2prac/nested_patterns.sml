(* Transfrom a triple of lists to a list of triples *)
(* ([1,2,3], [4,5,6], [7,8,9])  =>  [(1,4,7),(2,5,8), (3,6,9)]*)

exception ListLengthMismatch

fun zip list_triple =
    case list_triple of
        ([],[],[]) => []
        | (h1::t1, h2::t2, h3::t3) => (h1,h2,h3) :: zip (t1,t2,t3)
        | _ => raise ListLengthMismatch

fun unzip list =
    case list of
        [] => ([],[],[])
        | (a,b,c) :: tl =>
            let val (l1,l2,l3) = unzip tl
            in (a::l1, b::l2, c::l3)
            end

val  a = ([1,2,3], [4,5,6], [7,8,9])
