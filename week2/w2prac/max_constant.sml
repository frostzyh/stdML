datatype exp = 
            Constant of int
            | Negate of exp
            | Add of exp * exp
            | Multiply of exp * exp
            

(* Working but inefficent implementation *)            
fun max_constant e =
    case e of 
        Constant i => i
        | Negate e2 => max_constant e2
        | Add(e1, e2) => if max_constant e1 > max_constant e2
                         then max_constant e1
                         else max_constant e2
        | Multiply(e1,e2) => 
                        if max_constant e1 > max_constant e2
                        then max_constant e1
                        else max_constant e2
        
        
val test_exp = Add (Constant 19, Negate (Constant 4))
val nineteen = max_constant test_exp 

(* Elegant implementation *)
fun max_constant2 e =
    case e of 
        Constant i => i
        | Negate e2 => max_constant2 e2
        | Multiply(e1, e2) => Int.max(max_constant2 e1,max_constant2 e2)
        | Add(e1, e2) => Int.max(max_constant2 e1,max_constant2 e2)

val test_exp2 = Add (Constant 19, Negate (Constant 4))
val nineteen2 = max_constant2 test_exp2
