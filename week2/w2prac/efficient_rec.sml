(* infficient factorial fucntion *)

fun fact1 n =
  if n = 0
  then 1
  else n * fact1(n - 1)


(* Efficient implementation *)
fun fact n =
  let fun aux (n,acc) =
    if n = 0
    then acc
    else aux (n-1, acc*n)
  in
    aux (n,1)
  end

(* This implementation is more efficient because it returns the calling function
directly, so the call stack would always has one element on the stack. The prior
implementation would contain 4 items on the stack at certain point when fact1 3
is called :
fact1 3
fact1 2
fact1 1
fact1 0

For the efficient impleentation, because ML recognizes the tail calls, so it
treats them differently:
    - pop the caller from the stack before the call, allowing callee to resue
    the same stack space
    - (Along with other optimizations) as efficient as a loop (iteration)
*)
