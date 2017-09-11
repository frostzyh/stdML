(* Each of *)

(* Triple *)
fun sum_triple triple =
    case triple of
        (x,y,z) => x + y + z

(* Record *)
fun full_name r =
    case r of
        {first = x, middle = y, last = z} =>
            x ^ " " ^ y ^ " " ^ z

(* improved versions *)

fun sum_triple triple =
    let val (x,y,z) = triple
    in x + y + z
    end

fun full_name r =
    let val {first = x, middle = y, last = z} = r
    in x ^ " " ^ y ^ " " ^ z
    end

(* more improvement *)
fun sum_triple (x,y,z) = x + y + z

fun full_name {first = x, middle = y, last = z} =
    x ^ " " ^ y ^ " " ^ z
