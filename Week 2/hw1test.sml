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