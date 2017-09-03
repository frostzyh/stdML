(* The "one of " type *)

datatype mytype = TwoInts of int * int
                | Str of string
                | Pizza
(*               
val a = Str "hi"
val b = str
val c = Pizza
val d = TwoInts(1+2, 3+4)
val e = a
*) 

(* mytype -> int *)
fun f (x : mytype) = 
    case x of
        Pizza => 3
        | Str s => 8
        | TwoInts(i1,i2) => i1 + i2;

f Pizza;
f (Str "Hello");
f (TwoInts (7,9));
