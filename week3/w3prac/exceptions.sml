fun hd xs =
  case xs of
    [] => raise List.Empty
    | x::_ => x

(* This is customzed exception *)
exception MyNoDivide of int

fun mydiv (x,y) =
  if y = 0
  then raise MyNoDivide (22)
  else x div y

fun maxlist (xs, ex) =
  case xs of
    [] => raise ex
    | x::[] => x
    | x::xs' => Int.max(x, maxlist(xs',ex))

exception wiredException

val temp1 = maxlist ([3,4,5], wiredException)
    handle wiredException => ~1

val temp2 = maxlist ([], wiredException)
    handle wiredException => ~1

val temp3 = mydiv (5,0) handle MyNoDivide(22) => ~2
