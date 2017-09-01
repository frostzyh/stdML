(* use "Assignment1.sml"; *)
(* use "A1_test.sml"; *)


use "Assignment1.sml";

val test1 = is_older ((1,2,3),(2,3,4)) = true

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3

val test9 = what_month 70 = 3

val test10 = month_range (31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)

val testSort = sort_list([5,7,3,1,7,42,16,74,15])

val testSort2 = sort_list([5,7,3,1,7,42,3,16,22,7,74,15])
val testRemoveDup = remove_dup([5,7,3,1,7,42,3,16,22,7,74,15])

val chan1 = number_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val chan2 = dates_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val chan3_1 = reasonable_date (2012,2,29)

val chan3_2 = reasonable_date (2012,13,29)

val chan3_3 = reasonable_date (2012,12,29)

val chan3_4 = reasonable_date (1912,11,31)
