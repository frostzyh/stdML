datatype exp =
            Constant of int
            | Negate of exp
            | Add of exp * exp
            | Multiply of exp * exp

fun eval_old e =
    case e of
        Constant i => i
        | Negate e2 => ~ (eval_old e2)
        | Add(e1, e2) => (eval_old e1) + (eval_old e2)
        | Multiply(e1,e2) => (eval_old e1) * (eval_old e2)

(* fun funcitonName pattern = expression *)

fun eval (Constant i) = i
  | eval (Negate e2) = ~ (eval e2)
  | eval (Add(e1,e2)) = (eval e1) + eval (e2)
  | eval (Multiply(e1,e2)) = (eval e1) * (eval e2)

fun append ([],ys) = ys
  | append (x::xs', ys) = x :: append(xs', ys)
