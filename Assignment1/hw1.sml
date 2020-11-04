fun is_older(a: int*int*int, b: int*int*int) =
    #1 b > #1 a
    orelse (#1 b = #1 a
	    andalso (#2 b > #2 a
		     orelse (#2 b = #2 a
			     andalso #3 b > #3 a)));


fun number_in_month([], _) = 0
  | number_in_month((_,n,_)::d,m) = (if m=n then 1 else 0)+number_in_month(d,m);

fun number_in_months(d,[]) = 0
  | number_in_months(d,m::l) = number_in_month(d,m)+number_in_months(d,l);

fun dates_in_month([],_) = []
  | dates_in_month((y,m,d)::l,n) = (if m=n then [(y,m,d)] else [])@dates_in_month(l,n);

fun dates_in_months(_,[]) = []
  | dates_in_months(d,m::l) = dates_in_month(d,m)@dates_in_months(d,l);

fun get_nth(l,1)=hd l
  | get_nth(h::t,n)=get_nth(t,n-1);

fun date_to_string(d: int*int*int) =
    let
	val months = ["January","February","March","April","May","June","July","August","September","October","November","December"];
    in
	get_nth(months, #2 d)^" "^Int.toString(#3 d)^", "^Int.toString(#1 d)
    end;

fun number_before_reaching_sum(_,[]) = 0
  | number_before_reaching_sum(s,h::t) = if h<s then 1+number_before_reaching_sum(s-h,t) else 0;

fun what_month(d: int) =
    1+number_before_reaching_sum(d, [31,28,31,30,31,30,31,31,30,31,30,31]);

fun month_range(d1, d2) =
    if d1<=d2
    then
	what_month(d1)::month_range(d1+1,d2)
    else
	[];

fun oldest([]) = NONE
  | oldest(d::l) =
    let
	fun m(x,[]) = x
	  | m(x, d::l) =
	    if is_older(x, d)
	    then m(x, l)
	    else m(d, l)
    in
	SOME(m(d,l))
    end;
