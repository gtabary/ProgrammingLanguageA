(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

			       (**** you can put all your code here ****)

val only_capitals = List.filter (fn s => Char.isUpper(String.sub(s,0)));

val longest_string1 = List.foldl (fn(s1, s2) => if String.size(s1) > String.size(s2) then s1 else s2) "";

val longest_string2 = List.foldl (fn(s1, s2) => if String.size(s1) >=String.size(s2) then s1 else s2) "";

fun longest_string_helper f = List.foldl (fn(s1, s2) => if f(String.size(s1), String.size(s2)) then s1 else s2) "";
val longest_string3 = longest_string_helper (fn(s1, s2) => s1>s2);
val longest_string4 = longest_string_helper (fn(s1, s2) => s1>=s2);

val longest_capitalized = longest_string1 o only_capitals;

val rev_string = implode o List.rev o explode;

exception NoAnswer
fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
      | x::xs' =>
	case f x of
	    SOME v => v
	  | NONE => first_answer f xs'; 
			       
fun all_answers f xs = 
    let
	fun join(acc, []) = acc
	  | join(NONE, _) = NONE 
	  | join(SOME l, x::xs') =
	    case f x of
		NONE => NONE
	      | SOME v => join(SOME (v@l), xs')
    in
	join(SOME [], xs)
    end;

val count_wildcards = g (fn _ => 1) (fn _ => 0)

val count_wild_and_variable_lengths = g (fn _ => 1) String.size

fun count_some_var(s, p) = g (fn _ => 0) (fn s' => if s=s' then 1 else 0) p 


fun check_pat p =
    let
	fun patternsToNames(acc, []) = acc
	  | patternsToNames(acc, p::xp) = patternsToNames(
		case p of
		    Variable x         => x::acc
		  | TupleP ps         => acc@patternsToNames([], ps)
		  | ConstructorP(_,p) => acc@patternsToNames([], [p])
		  | _                 => acc,
		xp)
	fun isDistinct(x::xs) = isDistinct(xs) andalso not(List.exists (fn y => x=y) xs)
	  | isDistinct(_) = true 
    in
	isDistinct(patternsToNames([], [p]))
    end

fun match(v, p) =
    case (v,p) of
	(_,Wildcard) => SOME []
      | (_, Variable s) => SOME [(s, v)]
      | (Unit, UnitP) => SOME []
      | (Const i1, ConstP i2) => if i1=i2 then SOME [] else NONE
      | (Tuple vs, TupleP ps) => if List.length vs = List.length ps
				 then all_answers match (ListPair.zip(vs, ps))
				 else NONE
      | (Constructor (s1,v1), ConstructorP (s2,p2)) => if s1=s2 then match(v1, p2) else NONE
      | _ => NONE;

fun first_match v ps =
    SOME (first_answer (fn p => match(v, p)) ps) handle NoAnswer => NONE;
