(* fn : ('a -> 'a) * int * 'a -> 'a *)
fun n_times (f, n, x) =
    if n = 0
    then x
    else f(n_times(f,n-1,x))

(* *)
fun triple_n_times (n,x) = n_times((fn y => 3 * y), n, x)
val test1 = triple_n_times (2, 6)

fun add_n_times (n,x) = n_times((fn y => y + 1), n, x)
val test2 = add_n_times (10, 5)

fun nth_tail (n,x) = n_times(tl, n, x)
val test3 = nth_tail (3, [1,2,3,4,5])

val reverse_list = List.rev
val test4 = reverse_list [1,2,3,4,5]
