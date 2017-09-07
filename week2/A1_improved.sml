(* Author: Yehui Zhang *)
(* Date: Sep 1st, 2017 *)


(*Date : int*int*int (Year, month, day) *)

(* #1 *)    
fun is_older (date1 : int*int*int, date2 : int*int*int) =
    let
        val y1 = #1 date1
        val y2 = #1 date2
        val m1 = #2 date1
        val m2 = #2 date2
        val d1 = #3 date1
        val d2 = #3 date2
    in
        y1 < y2 orelse (y1 = y2 andalso m1 < m2)
            orelse (y1 = y2 andalso m1 = m2 andalso d1 < d2)
    end


fun number_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then 0
    else (if #2 (hd dates) = month then 1 else 0) 
          + number_in_month(tl dates, month)
          
fun number_in_months(dates : (int * int * int) list, months : int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then []
    else if #2 (hd dates) = month
    then (hd dates)::dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)
        
fun dates_in_months(dates : (int * int * int) list, months : int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(* Here is the challenging problems *)

(* check is element e is a duplicate in list *)
fun isDup (e : int, l : int list) = 
    not (null l) andalso ( e = hd l orelse isDup(e, tl l))

fun remove_dup (li : int list) = 
    if null li
    then []
    else
        let
            val tail = remove_dup(tl li)
        in
            if isDup(hd li, tail)
            then tail
            else (hd li) :: tail
        end


fun number_in_months_challenge (ldts : (int*int*int) list, months: int list) =
    number_in_months(ldts, remove_dup(months))
    
fun dates_in_months_challenge (ldts : (int*int*int) list, months: int list) =
    dates_in_months(ldts, remove_dup(months))
    

fun reasonable_date (d: int*int*int) =
    let
        val year = #1 d
        val month = #2 d
        val day = #3 d
        
        val isLeap = (year mod 400 = 0) orelse (year mod 4 = 0 andalso
            year mod 100 <> 0)
            
        val feburary = if isLeap then 29 else 28
        val lengths = [31,feburary,31,30,31,30,31,31,30,31,30,31]
        
        fun get_nth2 ( m: int, months: int list) =
            if m = 1
            then hd months
            else
                get_nth2(m-1, tl months)
    in
        year > 0 andalso month > 0 andalso month <= 12 andalso day > 0
            andalso day <= get_nth2(month,lengths)
    end
