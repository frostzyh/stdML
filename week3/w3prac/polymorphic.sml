
(* int list -> int *)
fun sum_list xs =
    case xs of
        [] => 0
        | x::xs' => x + sum_list xs'

(*  'a list * 'a list -> 'a list  *)
fun append (xs, ys) =
    case xs of
        [] => ys
        | x::xx => x :: append(xx, ys)

datatype 'a option = NONE | SOME of 'a

datatype 'a mylist = Empty | Cons of 'a * 'a mylist

datatype ('a, 'b) tree = Node of 'a * ('a, 'b) tree * ('a, 'b) tree
                        | Leaf of 'b

fun sum_tree tr =
    case tr of
        Leaf i => i
        | Node(i,left,right) => i + sum_tree left + sum_tree right

fun sum_leaves tr =
    case tr of
        Leaf i => i
        | Node(i, left, right) => sum_leaves left + sum_leaves right

(* Counting number of leaves *)
fun num_leaves tr =
    case tr of
        Leaf i => 1
        | Node(i, left, right) => num_leaves left + num_leaves right


(* Here is a datatype which is a binary tree that can hold no
data on its internal nodes, but each leaf can hold one of two
different types of data *)
datatype ('a, 'b) flower =
    Node of ('a, 'b) flower * ('a, 'b) flower
    | Leaf of 'a
    | Petal of 'b
