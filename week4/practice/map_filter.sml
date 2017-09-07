fun map (f,xs) =
    case xs of
        [] => []
        | x::xs' => (f x) :: (map (f,xs'))

fun filter (f,xs) =
    case xs of
        [] => []
        | x::xs' =>
            if f x
            then x::(filter (f,xs'))
            else filter(f,xs')

(* Add one to all *)
val test1 = map ((fn x => x+1), [2,4,6,7])

val test2 = map (hd,[[1,2],[3,4],[5,6,7]])

fun is_even v = (v mod 2 = 0)
(* fn : int list -> int list *)
fun all_even xs = filter (is_even, xs)
val test3 = all_even [1,12,5,32,7,34,21,53,22]

(* fn : ('a * int) list -> ('a * int) list *)
fun all_even_snd xs = filter ((fn (_,v) => is_even v), xs)
val test4 = all_even_snd [(2,3),(4,5),(9,8),(7,6)]
