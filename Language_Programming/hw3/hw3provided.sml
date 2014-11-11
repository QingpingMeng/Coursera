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

fun only_capitals xs=
  case xs of
       [] => []
     |x::xs'=> if Char.isUpper(String.sub(x,0)) 
               then x::only_capitals(xs') 
               else only_capitals(xs')

fun longest_string1 xs=foldl (fn (x,y)=>if size(y)>size(x) then y else x) "" xs

fun longest_string2 xs=foldl (fn (x,y)=>if size(y)>=size(x) then y else x) "" xs

fun longest_string_helper f xs =foldl (fn(s1,s2)=>if f(size(s1),size(s2)) then
  s1 else s2) "" xs

val longest_string3=longest_string_helper(fn(s1,s2)=>if s1>s2 then true else false)
val longest_string4=longest_string_helper(fn(s1,s2)=>if s1>=s2 then true else
  false)


val longest_capitalized =longest_string4 o only_capitals

val rev_string=implode o rev o explode

fun first_answer f xs=
  case xs of
       []=>raise NoAnswer
     |x::xs'=>case f x of
                   SOME y=>y
                 |NONE =>first_answer f xs'

fun all_answers f xs =
let fun helper l acc=
case l of
     []=>SOME acc
   |x::xs'=>case f x of
                NONE=>NONE
              |SOME y=>helper xs' (y@acc)
in
  helper xs []
end

val count_wildcards=g (fn _=>1) (fn _=>0)
val count_wild_and_variable_lengths=g (fn _=>1) (fn x=>size(x))
fun count_some_vars s p=g (fn _=>0) (fn x=>if x=s then 1 else 0) p

fun check_pat p =
let 
  fun helper1 pa=
    case pa of
         Variable x =>[x]
       |TupleP s=> foldl (fn(p,i)=>i@helper1 p) [] s
       |ConstructorP(s,pat) => [s]@helper1 pat
       |_ => []

  fun helper2 l=
    case l of 
         []=>true
       |x::[]=>true
       |x::y::xs'=> (List.exists (fn y=> x<>y) xs')andalso helper2 xs'
in
 helper2(helper1 p)
end 

fun match (v,p) =
  case (v,p) of 
       (_,Wildcard)=>SOME []
     |(v,Variable s)=>SOME [(s,v)]
     |(Unit,UnitP) => SOME []
     |(Const x,ConstP y) => if x=y then SOME [] else NONE
     |(Tuple vs, TupleP ps) =>all_answers( fn(x,y) =>match(x,y))
     (ListPair.zip(vs,ps))
     |(Constructor(n,value),ConstructorP(s,pat)) => if n=s then match(value,pat)
                                                   else NONE
     |_=>NONE

fun first_match v ps =
  case ps of
       []=>NONE
     |x::xs=>SOME (first_answer (fn (x,y) => match(x,y)) [(v,x)])
      handle NoAnswer => (first_match v xs)
