(* Author: Yehui Zhang *)
(* Date: Sep 1st, 2017 *)


(*Date : int*int*int (Year, month, day) *)

fun is_older (d1 : int*int*int, d2 : int*int*int) =
    if #1 d1 < #1 d2
    then true
    else if #1 d1 > #1 d2
    then false
    else if #2 d1 < #2 d2
    then true
    else if #2 d1 > #2 d2
    then false
    else if #3 d1 < #3 d2
    then true
    else false

fun number_in_month (ldts : (int*int*int) list, month: int) =
    if null ldts
    then 0
    else
        if #2 (hd ldts) = month
        then 1+ number_in_month (tl ldts, month)
        else number_in_month (tl ldts, month)

fun number_in_months (ldts : (int*int*int) list, months: int list) =
    if null ldts orelse null months
    then 0
    else
        number_in_month (ldts, hd months) + number_in_months (ldts, tl months)

fun dates_in_month (ldts : (int*int*int) list, month: int) =
    if null ldts
    then []
    else
        if #2 (hd ldts) = month
        then (hd ldts) :: dates_in_month(tl ldts, month)
        else
            dates_in_month(tl ldts, month)

fun dates_in_months (ldts : (int*int*int) list, months: int list) =
    if null ldts orelse null months
    then []
    else
        dates_in_month(ldts, hd months) @ dates_in_months(ldts, tl months)

fun get_nth (strs : string list, n: int) = 
    if null strs orelse n < 1
    then ""
    else
        if n = 1
        then hd strs
        else
            get_nth(tl strs, n-1)

fun date_to_string (date : int*int*int)=
    let
        val months = ["January", "February", "March", "April", 
        "May", "June", "July", "August", "September", "October",
        "November", "December"]
        val year = Int.toString(#1 date)
        val day = Int.toString(#3 date)
    in
        get_nth(months, #2 date) ^ " " ^ day ^ ", " ^ year
    end

fun number_before_reaching_sum (sum: int, nums : int list) =
    if null nums
    then ~1000
    else
        if hd nums >= sum
        then 0
        else
            1 + number_before_reaching_sum(sum - hd nums, tl nums)

fun what_month (day : int) = 
    if day < 1 orelse day > 365
    then 0
    else
        let
            val months = [31,28,31,30,31,30,31,31,30,31,30,31]
        in
            number_before_reaching_sum(day, months) + 1
        end
        
fun month_range (d1 : int, d2 : int) = 
    if d1 > d2
    then []
    else
        what_month(d1) :: month_range(d1+1, d2)

fun oldest (ldts : (int*int*int) list) = 
    if null ldts
    then NONE
    else
        let
            fun oldest_noempty(ldts : (int*int*int) list) =
                if null (tl ldts)
                then hd ldts
                else
                    let
                        val tail = oldest_noempty(tl ldts)
                    in
                        if is_older(hd ldts, tail)
                        then  hd ldts
                        else tail
                    end
        in 
            SOME (oldest_noempty (ldts))
        end




(* Here is the challenging problems *)

(* Insertion sort: return a sorted list *)
fun insertion (e : int, l : int list) = 
    if null l
    then [e]
    else
        if hd l > e
        then e :: l
        else
            hd l :: insertion(e, tl l)

fun sort_list (li : int list) =
    if null li
    then []
    else
        insertion(hd li, sort_list(tl li))

fun remove_dup (li : int list) = 
    if null li
    then li
    else
        let
            val li = sort_list(li)
            fun helper(prev : int, li : int list) = 
                if null li
                then []
                else
                    if prev = (hd li)
                    then helper(prev, tl li)
                    else hd li :: helper(hd li, tl li)
        in
            (hd li) :: helper(hd li, tl li)
        end


fun number_in_months_challenge (ldts : (int*int*int) list, months: int list) =
    number_in_months(ldts, remove_dup(months))
    
fun dates_in_months_challenge (ldts : (int*int*int) list, months: int list) =
    dates_in_months(ldts, remove_dup(months))
    

fun reasonable_date (d: int*int*int) =
    let
        fun isLeapYear(year : int) =
            let
                val con1 = (#1 d) mod 400 = 0
                val con2 = ((#1 d) mod 4 = 0) andalso ((#1 d) mod 100 <> 0)
            in con1 orelse con2
            end
        
        val leapMonth = [31,29,31,30,31,30,31,31,30,31,30,31]
        val normMonth = [31,28,31,30,31,30,31,31,30,31,30,31]
        
        fun get_nth2 ( m: int, months: int list) =
            if m = 1
            then hd months
            else
                get_nth2(m-1, tl months)
    in
        if #1 d < 1 orelse #2 d < 1 orelse #2 d > 12
        then false
        else
            if isLeapYear(#1 d)
            then 
                #3 d <= get_nth2(#2 d, leapMonth)
            else
                #3 d <= get_nth2(#2 d, normMonth)
    end
