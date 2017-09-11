
(* fn : ('a * 'b -> 'a) * 'a * 'b list -> 'a  *)
fun fold (f, acc, xs) =
    case xs of
        [] => acc
        | x::xs' => fold (f, f(acc,x), xs')


(* in (x,y)  x is the accumulator and y is the element of the list*)
(* functions not using private data *)
(* sum *)
fun f1 xs = fold ((fn (x,y) => x + y), 0, xs)

(* are all elements non-negative *)
fun f2 xs = fold ((fn (x,y) => x andalso y >= 0), true, xs)

(* functions using private data *)
(* Number of elements that are between lo and hi, inclusive *)
fun f3 (xs, lo, hi) =
    fold ((fn (x,y) => x + (if y >= lo andalso y <= hi then 1 else 0)), 0, xs)

(* is all elements shorter than string s*)
fun f4 (xs, s) =
    let
        val i = String.size s
    in
        fold ((fn (x,y) => x andalso String.size y < i), true, xs)
    end

fun f5 (g, xs) =
    fold ((fn (x,y) => x andalso g y), true, xs)

fun f4again (xs, s) =
    let
        val i = String.size s
    in f5 (fn y => String.size y < i, xs)
    end
