(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun all_except_option(_, []) = NONE
  | all_except_option(s, x::l) =
    if s=x
    then
      SOME l
    else
      case all_except_option(s, l) of
        NONE => NONE
       | SOME il => SOME (x::il);
	
fun get_substitutions1([], s) = []
  | get_substitutions1(x::l, s) =
    let
      val j = get_substitutions1(l, s)
    in
      case all_except_option(s, x) of
        NONE => j
      | SOME il => il@j
    end;

fun get_substitutions2(lst, s) =
    let
      fun get_substitutions2'([], _, a) = a
	| get_substitutions2'(x::l, s, a) =
          case all_except_option(s, x) of
            NONE => get_substitutions2'(l, s, a)
            | SOME il => get_substitutions2'(l, s, il@a)
    in
      get_substitutions2'(lst, s, [])
    end;

fun similar_names(
    names,
    { first=f, last=l, middle=m }) =
    let
      fun similar_names2'([], a) = a
        | similar_names2'(x::lst, a)  =
          similar_names2'(lst, {first=x, last=l, middle=m}::a)
    in
      similar_names2'(get_substitutions2(names, f), [{ first=f, last=l, middle=m}])
    end;

fun card_color(Spades, _) = Black
  | card_color(Clubs, _) = Black
  | card_color(Hearts, _) = Red
  | card_color(Diamonds, _) = Red;

fun card_value(_, Ace) = 11
  | card_value(_, Jack) = 10
  | card_value(_, Queen) = 10
  | card_value(_, King) = 10
  | card_value(_, Num i) = i;

fun remove_card(cs, c, e) =
    case all_except_option(c, cs) of
      NONE => raise e
     | SOME l => l;

fun all_same_color([]) = true
  | all_same_color((_,_)::[]) = true
  | all_same_color((_,c1)::(s, c2)::cs) =
    c1=c2 andalso all_same_color((s,c2)::cs);
		        
fun sum_cards(cs) =
    let
      fun sum_cards'([], s) = s
        | sum_cards'(c::cs, s) = sum_cards'(cs, s+card_value(c))
    in
	sum_cards'(cs, 0)
    end;

fun score(cs, g) =
    let
	val s = sum_cards(cs)
	val p = if s > g
		then 3*(s-g)
	        else g-s 
    in
	if all_same_color(cs)
	then p div 2
	else p
    end;

fun officiate(cs, ms, g) =
    let
	fun officiate'(hs', cs', []) = score(hs', g)
	  | officiate'(hs', cs', Discard c::ms') =
	    officiate'(remove_card(hs', c, IllegalMove), cs', ms')
	  | officiate'(hs', [], Draw::ms') = score(hs', g)
	  | officiate'(hs', c::cs', Draw::ms') =
	    if sum_cards(c::hs') > g
	    then score(c::hs', g)
	    else officiate'(c::hs', cs', ms')
    in
	officiate'([], cs, ms)
    end;


fun score_challenge(cs, g) =
    let
	fun append_css'(c, [], ncss) = ncss
	  | append_css'(c, css::ocss, ncss) = append_css'(c, ocss, (c::css)::ncss)
	fun append_css(c, css) = append_css'(c, css, [])
	fun build_css'([], css) = css
	  | build_css'(c::cs', css) =
	    case c of
              (s,Ace) => build_css'(cs', append_css(c, css)@append_css((s,Num 1), css))
             |(_,_)  => build_css'(cs', append_css(c, css))
	fun build_css(cs) = build_css'(cs, [[]])
	fun min_score([]) = score([], g)
	  | min_score(cs'::[]) = score(cs', g) 
	  | min_score(cs'::css) = Int.min(score(cs', g), min_score(css))
    in
	min_score(build_css(cs))
    end;

fun build_css_challenge(cs) =
    let
	fun append_css'(c, [], ncss) = ncss
	  | append_css'(c, css::ocss, ncss) = append_css'(c, ocss, (c::css)::ncss)
	fun append_css(c, css) = append_css'(c, css, [])
	fun build_css'([], css) = css
	  | build_css'(c::cs', css) =
	    case c of
              (s,Ace) => build_css'(cs', append_css(c, css)@append_css((s,Num 1), css))
             |(_,_)  => build_css'(cs', append_css(c, css))
    in
	build_css'(cs, [[]])
    end;

fun min_challenge([], f) = f([])
  | min_challenge(cs::[], f) = f(cs)
  | min_challenge(cs::css, f) = Int.min(f(cs), min_challenge(css, f));

fun score_challenge(cs, g) =
    let
	fun f(x) = score(x, g)
    in
        min_challenge(build_css_challenge(cs), f)
    end;

fun officiate_challenge(cs, ms, g) =
    let
	fun f(x) = officiate(x, ms, g)
    in
        min_challenge(build_css_challenge(cs), f)
    end;

