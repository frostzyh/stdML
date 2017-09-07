
fun nondecreasing xs =
  case xs of
    [] => true
    | x :: xs' =>
      case xs' of
        [] => true
        | y :: ys' => x <= y andalso nondecreasing xs'


fun nondecreasing2 xs =
  case xs of
    [] => true
    | _::[] => true
    | x::(y::z) => x <= y andalso nondecreasing2 (y::z)

(* P = positive, N = negative, Z = zero *)
datatype sgn = P | N | Z

fun multsign (x1, x2) =
    let fun sign x = if x = 0 then Z else if x > 0 then P else N
    in
      case (sign x1, sign x2) of
        (Z,_) => Z
        | (_,Z) => Z
        | (P,P) => P
        | (N,N) => P
        | _ => N
        (*
        | (P,N) => N
        | (N,P) => N
        *)
    end


fun len xs =
  case xs of
    [] => 0
    | _::xs' => 1 + len xs'
