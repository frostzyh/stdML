fun sum1 xs =
  case xs of
    [] => 0
    | x::xs' => x + sum1 xs'


(* improved by using tail resursion *)
fun sum xs =
  let
    fun helper (xs, acc) =
      case xs of
        [] => acc
        | x :: xs' => helper (xs', acc + x)
  in
    helper (xs, 0)
  end


val t1 = sum1 [1,2,3,4,5,6,7]

val t2 = sum [1,2,3,4,5,6,7]


(* bad implementaion. Use Quadratic time beacuse of using @
  size=k]@[size=1] => k operataions *)
fun rev2 xs =
  case xs of
    [] => []
    | x :: xs' => (rev2 xs') @ [x]

(* Much better impletation. Linear time. stack size always keep at 1*)
fun rev xs =
  let
    fun helper (xs, tail) =
      case xs of
        [] => tail
        | x :: xs' => helper(xs', x::tail)
  in
    helper (xs, [])
  end

val t3 = rev2 ["a","b","c"]
val t4 = rev ["a","b","c"]
