(* Helpers *)

fun days_in_date (d: int*int*int) = 
  #1 d * 365 + ((#2 d - 1) * 31) - (if #2 d > 2 then (#2 d div 2) else 0) + #3 d
   - (if #2 d > 2 then 2 else 0) (* February is 28 days *)
   + (if #2 d > 7 then 1 else 0) (* July and August both have 31 days *)

(* Target functions  *)

(* 1 *)
(* Write a function is_older that takes two dates and evaluates to true or false. It evaluates to true if the first argument is a date that comes before the second argument. (If the two dates are the same, the result is false.) *)
fun is_older (first: int*int*int, second: int*int*int) =
  days_in_date(first) < days_in_date(second)

(* 2 *)
(* Write a function number_in_month that takes a list of dates and a month (i.e., an int) and returns how many dates in the list are in the given month. *)
fun number_in_month (ds: (int*int*int) list, month: int) =
  if null ds
  then 0
  else let fun date_in_month (d: int*int*int, month: int) =
             if #2 d = month then 1 else 0
       in
         date_in_month(hd ds, month) + number_in_month(tl ds, month)
       end

(* 3 *)
(* Write a function number_in_months that takes a list of dates and a list of months (i.e., an int list) and returns the number of dates in the list of dates that are in any of the months in the list of months. Assume the list of months has no number repeated. Hint: Use your answer to the previous problem. *)
fun number_in_months (ds: (int*int*int) list, ms: int list) =
  if null ms
  then 0
  else number_in_month(ds, hd ms) + number_in_months(ds, tl ms)

(* 4 *)
(* Write a function dates_in_month that takes a list of dates and a month (i.e., an int) and returns a list holding the dates from the argument list of dates that are in the month. The returned list should contain dates in the order they were originally given. *)
fun dates_in_month (ds: (int*int*int) list, month: int) =
  if null ds
  then []
  else if #2 (hd ds) = month
       then hd ds :: dates_in_month(tl ds, month)
       else dates_in_month(tl ds, month)

(* 5 *)
(* Write a function dates_in_months that takes a list of dates and a list of months (i.e., an int list) and returns a list holding the dates from the argument list of dates that are in any of the months in the list of months. Assume the list of months has no number repeated. Hint: Use your answer to the previous problem and SML’s list-append operator (@). *)
fun dates_in_months (ds: (int*int*int) list, ms: int list) =
  if null ds orelse null ms
  then []
  else dates_in_month(ds, hd ms) @ dates_in_months(ds, tl ms)

(* 6 *)
(* Write a function get_nth that takes a list of strings and an int n and returns the nth element of the list where the head of the list is 1st. Do not worry about the case where the list has too few elements: your function may apply hd or tl to the empty list in this case, which is okay. *)
fun get_nth (ss: string list, n: int) =
  if n = 1 orelse n = 0
  then hd ss
  else get_nth(tl ss, n - 1)
  
(* 7 *)
(* Write a function date_to_string that takes a date and returns a string of the form January 20, 2013 (for example). Use the operator ^ for concatenating strings and the library function Int.toString for converting an int to a string. For producing the month part, do not use a bunch of conditionals. Instead, use a list holding 12 strings and your answer to the previous problem. For consistency, put a comma following the day and use capitalized English month names: January, February, March, April, May, June, July, August, September, October, November, December. *)
fun date_to_string (d: int*int*int) = 
  get_nth(["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"], #2 d) ^ " " ^ Int.toString(#3 d) ^ ", " ^ Int.toString(#1 d)

(* 8 *)
(* Write a function number_before_reaching_sum that takes an int called sum, which you can assume is positive, and an int list, which you can assume contains all positive numbers, and returns an int. You should return an int n such that the first n elements of the list add to less than sum, but the first n + 1 elements of the list add to sum or more. Assume the entire list sums to more than the passed in value; it is okay for an exception to occur if this is not the case. *)
fun number_before_reaching_sum (sum: int, i: int list) =
  if sum <= hd i
  then 0
  else 1 + number_before_reaching_sum(sum - hd i, tl i)

(* 9 *)
(* Write a function what_month that takes a day of year (i.e., an int between 1 and 365) and returns what month that day is in (1 for January, 2 for February, etc.). Use a list holding 12 integers and your answer to the previous problem. *)
fun what_month (d: int) = 
  number_before_reaching_sum(d, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]) + 1

(* 10 *)
(* Write a function month_range that takes two days of the year day1 and day2 and returns an int list [m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the month of day day2. Note the result will have length day2 - day1 + 1 or length 0 if day1>day2. *)
fun month_range (d1: int, d2: int) =
  if d1 > d2
  then []
  else what_month d1 :: month_range(d1 + 1, d2)

(* 11 *)
(* Write a function oldest that takes a list of dates and evaluates to an (int*int*int) option. It evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the list. *)
fun oldest (xs: (int*int*int) list) =
  if null xs
  then NONE
  else 
  let val tl_ans = oldest(tl xs)
	in if isSome tl_ans andalso days_in_date(valOf tl_ans) < days_in_date(hd xs)
	   then tl_ans
	   else SOME (hd xs)
	end

(* 12 *)
(* Challenge Problem: Write functions number_in_months_challenge and dates_in_months_challenge that are like your solutions to problems 3 and 5 except having a month in the second argument multiple times has no more effect than having it once. (Hint: Remove duplicates, then use previous work.) *)

(* 13 *)
(* Challenge Problem: Write a function reasonable_date that takes a date and determines if it describes a real date in the common era. A “real date” has a positive year (year 0 did not exist), a month between 1 and 12, and a day appropriate for the month. Solutions should properly handle leap years. Leap years are years that are either divisible by 400 or divisible by 4 but not divisible by 100. (Do not worry about days possibly lost in the conversion to the Gregorian calendar in the Late 1500s.) *)

(* Tests *)

(* days_in_date *)
val days_in_date_test_1 = days_in_date ((1,3,1)) = 365 + 31 + 28 + 1
val days_in_date_test_2 = days_in_date ((1,2,10)) = 365 + 31 + 10
val days_in_date_test_3 = days_in_date ((1,8,10)) = 365 + (4 * 31) + (2 * 30) + 28 + 10
val days_in_date_test_4 = days_in_date ((1,7,10)) = 365 + (3 * 31) + (2 * 30) + 28 + 10
val days_in_date_test_5 = days_in_date ((0,12,31)) = 365

(* 1 *)
val is_older_test_1 = is_older ((1,2,3),(2,3,4)) = true
val is_older_test_2 = is_older ((1,1,1),(1,1,1)) = false
val is_older_test_3 = is_older ((3,2,3),(2,3,4)) = false
val is_older_test_4 = is_older ((0,0,0),(0,0,0)) = false
val is_older_test_5 = is_older ((1,2,3),(1,3,1)) = true

(* 2 *)
val number_in_month_test_1 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1
val number_in_month_test_2 = number_in_month ([(2012,2,28),(2013,2,1)],2) = 2
val number_in_month_test_3 = number_in_month ([(2012,2,28),(2013,12,1)],3) = 0
val number_in_month_test_4 = number_in_month ([(2012,2,28),(2013,2,1)],2) = 2
val number_in_month_test_5 = number_in_month ([(2012,1,31),(2013,3,1)],2) = 0
val number_in_month_test_6 = number_in_month ([],2) = 0

(* 3 *)
val number_in_months_test_1 = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val number_in_months_test_2 = number_in_months([(2012,12,28),(2013,12,1),(2011,12,31),(2011,12,28)],[2,3,4]) = 0
val number_in_months_test_3 = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[1,11]) = 0
val number_in_months_test_4 = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[]) = 0
val number_in_months_test_5 = number_in_months([],[1,2,3]) = 0
val number_in_months_test_6 = number_in_months([(2012,2,28)],[2,2]) = 2

(* 4 *)
val dates_in_month_test_1 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val dates_in_month_test_2 = dates_in_month ([(2012,12,28),(2013,12,1)],2) = []
val dates_in_month_test_3 = dates_in_month ([(2012,2,28),(2013,2,1)],2) = [(2012,2,28),(2013,2,1)]
val dates_in_month_test_4 = dates_in_month ([],2) = []

(* 5 *)
val dates_in_months_test_1 = dates_in_months([(2012,2,28),
                                              (2013,12,1),
                                              (2011,3,31),
                                              (2011,4,28)],[2,3,4]) = [(2012,2,28),
                                                                       (2011,3,31),
                                                                       (2011,4,28)]
val dates_in_months_test_2 = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[7]) = []

(* 6 *)
val get_nth_test_1 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"
val get_nth_test_2 = get_nth (["hi", "there", "how", "are", "you"], 5) = "you"
val get_nth_test_3 = get_nth (["hi", "there", "how", "are", "you"], 0) = "hi"

(* 7 *)
val date_to_string_test_1 = date_to_string (2013, 6, 1) = "June 1, 2013"

(* 8 *)
val number_before_reaching_sum_test_1 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3
val number_before_reaching_sum_test_2 = number_before_reaching_sum (11, [1,2,3,4,5]) = 4
val number_before_reaching_sum_test_3 = number_before_reaching_sum (15, [1,2,3,4,5]) = 4
val number_before_reaching_sum_test_4 = number_before_reaching_sum (0, [1,2,3,4,5]) = 0

(* 9 *)
val what_month_test_1 = what_month 70 = 3
val what_month_test_2 = what_month 1 = 1
val what_month_test_3 = what_month 365 = 12
val what_month_test_4 = what_month 31 = 1

(* 10 *)
val month_range_test_1 = month_range (31, 34) = [1,2,2,2]
val month_range_test_2 = month_range (1, 2) = [1,1]
val month_range_test_3 = month_range (364, 365) = [12,12]
val month_range_test_4 = month_range (3, 2) = []

(* 11 *)
val oldest_test_1 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
val oldest_test_2 = oldest([(2012,2,28),(2012,2,28)]) = SOME (2012,2,28)
val oldest_test_3 = oldest([]) = NONE

(* 12 *)
(* 13 *)