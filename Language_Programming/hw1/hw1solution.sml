(* function to evaluate para1 is older than para2 *)
fun is_older(yearFirst:int*int*int,yearSecond:int*int*int)=
  if (#1 yearFirst - #2 yearFirst)<0 then
        true
  else
    if (#)
        (* funtion to evaluate how many dates are in given month *)
fun number_in_month(dates: (int*int*int) list, month:int)=
  if null dates
  then 0
  else
    if #2 (hd(dates)) = month
    then 1 + number_in_month(tl dates,month)
    else 0 + number_in_month(tl dates,month)

fun number_in_months(dates: (int*int*int) list,months: int list)=
  if null dates orelse null months
  then 0
  else
    number_in_month(dates,hd months)+number_in_months(dates, tl months)

fun dates_in_month(dates: (int*int*int) list,month:int)=
  if null dates
  then []
  else
    if #2 (hd(dates)) = month
    then hd(dates)::dates_in_month(tl(dates),month)
    else dates_in_month(tl(dates),month)

fun dates_in_months(dates: (int*int*int) list,months: int list)=
  if null dates orelse null months
  then []
  else
    dates_in_month(dates,hd(months)) @ dates_in_months(dates, tl(months))

fun get_nth(strings: string list,order: int)=
  if order =1
  then hd(strings)
  else
    get_nth(tl(strings),order-1)

fun date_to_string(date: int*int*int)=
let val months =
["January","February","March","April","May","June","July","August","September","October","November","December"]
                                                  in
                                                    get_nth(months,#2 date)^" "^Int.toString(#3 date)^", "^Int.toString(#1 date)
end

fun number_before_reaching_sum(sum:int, numbers: int list)=
  if (sum - hd(numbers)) <= 0 then
    0
  else
    1+number_before_reaching_sum((sum - hd numbers),tl(numbers))
 

fun what_month(date:int)=
let 
  val days=[31,28,31,30,31,30,31,31,30,31,30,31]
in
  number_before_reaching_sum(date,days)+1
end

fun month_range(date1:int,date2:int)=
  if date1>date2 then
    []
  else
    what_month(date1)::month_range(date1+1,date2)

fun oldest(dates:(int*int*int) list)=
  if null dates then
    NONE
  else let 
    fun oldest_nonempty(dates:(int*int*int) list)=
      if null (tl dates) then
        hd dates
      else let val tl_ans=oldest_nonempty(tl dates)
in
  if is_older(hd dates,tl_ans) then hd dates
  else tl_ans
end
           in 
             SOME (oldest_nonempty(dates))
           end


