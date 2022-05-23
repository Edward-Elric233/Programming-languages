fun is_older(first: int*int*int, second: int*int*int) =
    (* return first < second *)
    #1 first < #1second orelse (#1 first = #1 second andalso #2 first < #2 second) orelse (#1 first = #1 second andalso #2 first = #2 second andalso #3 first < #3 second)

fun number_in_month(dates: (int*int*int) list, month: int) =
    (* return the number of date's month equal given month *)
    if null dates
    then 0
    else let
	val rest = number_in_month(tl dates, month);
    in
	if #2(hd dates) = month
	then rest + 1
	else
	    rest
    end;
     
fun number_in_months(dates: (int*int*int) list, months: int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months);

fun dates_in_month(dates: (int*int*int) list, month: int) =
    if null dates
    then []
    else let
	val rest = dates_in_month(tl dates, month);
	val head = hd dates;
    in
	if #2 head = month
	then head::rest
	else rest
    end;

fun dates_in_months(dates: (int*int*int) list, months: int list) =
    if null months
    then []
    else dates_in_month(dates, hd months)@dates_in_months(dates, tl months);


fun get_nth(ss: string list, n: int) =
    if n = 1
    then hd ss
    else get_nth(tl ss, n - 1);

val MonthsNames = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];

fun date_to_string(date: int*int*int) =
    get_nth(MonthsNames, #2 date)^" "^Int.toString(#3 date)^", "^Int.toString(#1 date);


fun number_before_reaching_sum(sum: int, numbers: int list) =
    let
	fun cal_sum(idx: int, nums: int list, cur: int) =
	    let val t = cur + hd nums; in
	    if t < sum
	    then cal_sum(idx + 1, tl nums, t)
	    else idx
	    end;
    in
	cal_sum(0, numbers, 0)
    end;

	
val DayOfMonths = [31,28,31,30,31,30,31,31,30,31,30,31];

fun what_month(day: int) =
    number_before_reaching_sum(day, DayOfMonths) + 1;

fun month_range(day1: int, day2: int) =
    if day1 > day2
    then []
    else what_month(day1)::month_range(day1 + 1, day2);


fun oldest(dates: (int*int*int) list) =
    if null dates
    then NONE
    else let
	fun oldest_nonempty(dates: (int*int*int) list)=
	    let
		val tail = tl dates;
		val head = hd dates;
	    in
		if null tail
		then head
		else let
			val rest = oldest_nonempty(tail);
		    in
			if is_older(head, rest)
				then head
				else rest
		    end
	    end;
    in
	SOME (oldest_nonempty dates)
    end;


fun remove_duplicates(months: int list) =
    if null months
    then []
    else let
	fun occur_in_list(month: int, months: int list) =
	    not(null months) andalso
	    (month = hd months orelse occur_in_list(month, tl months));
	val rest = remove_duplicates(tl months);
	val head = hd months;
    in
	if occur_in_list(head, rest)
	then rest
	else head::rest
    end;
		 
					  
			 
(*
(* for remove duplicates for date list *)
fun remove_duplicates(dates: (int*int*int) list) =
    if null dates
    then []
    else let
	fun is_equal(first: int*int*int, second: int*int*int) =
	    #1 first = #1 second andalso #2 first = #2 second andalso #3 first = #3 second;

	fun occur_in_list(date: int*int*int, dates: (int*int*int) list) =
	    not(null dates) andalso
	    (is_equal(date, hd dates) orelse occur_in_list(date, tl dates));

	val rest = remove_duplicates(tl dates);
	val head = hd dates;
    in
	if occur_in_list(head, rest)
	then rest
	else head::rest
    end;
*)
	


fun number_in_months_challenge(dates: (int*int*int) list, months: int list) =
    let
	val unduplicated_months = remove_duplicates months;
    in
	number_in_months(dates, unduplicated_months)
    end;
    

fun dates_in_months_challenge(dates: (int*int*int) list, months: int list) =
    let
	val unduplicated_months = remove_duplicates months;
    in
	dates_in_months(dates, unduplicated_months)
    end;

fun reasonable_date(date: int*int*int) =
    let
	val year = #1 date;
	val month = #2 date;
	val day = #3 date;
	fun is_leap(year: int) =
	    year mod 400 = 0 orelse year mod 4 = 0 andalso year mod 100 <> 0;
	fun get_nth_month_day(n: int, days: int list) =
	    if n = 1
	    then hd days
	    else get_nth_month_day(n - 1, tl days);
    in
	year > 0 andalso month > 0 andalso month <= 12 andalso day > 0
	andalso
	    let
		val monthDay = get_nth_month_day(month, DayOfMonths);
	    in
		if month = 2 andalso is_leap(year)
		then day <= monthDay + 1
		else day <= monthDay
	    end

    end;
		      
