(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
string), then you avoid several of the functions in problem 1 having
polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
  s1 = s2

  (* put your solutions for problem 1 here *)
fun all_except_option(str,lst)=
  case lst of
       [] => NONE
     |x::xs'=>if x=str 
         then SOME xs' 
         else case all_except_option(str,xs') of
                   NONE => NONE
                 |SOME y => SOME(x::y)

fun get_substitutions1 (xs,str)=
  case xs of
       [] =>[]
     |x::xs' => case all_except_option(str,x) of
                     NONE => []
                   |SOME y =>y@ get_substitutions1(xs',str)

fun get_substitutions2 (names,str)=
let 
  fun helper(xs,lst) = 
    case xs of
         [] => lst
       |x::xs'=> case all_except_option(str,x) of
                      NONE => helper(xs',lst)
                    |SOME y => helper(xs',y@lst)
                                                 in
                                                   helper(names,[])
end

fun similar_names (lst,name)=
let val {first=f,middle=m,last=l} =name
  fun aux(xs) =
    case xs of
         [] =>[name]
       |x::xs'=>{first=x,middle=m,last=l}::aux(xs')    
in
  aux(get_substitutions2(lst,f))
end


(* you may assume that Num is always used with values 2, 3, ..., 10
though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color (suit,rank) =
  case suit of
       Clubs => Black
     |Spades => Black
     |_ => Red

fun card_value (suit,rank)=
  case rank of
       Num i =>i
     |Ace => 11
     |_ =>10

fun remove_card(cs,c,e)=
  case all_except_option(cs,c) of
       NONE => raise e
     |SOME cards => cards

fun all_same_color cs =
  case cs of
       [] => true
     |x::[]=>true
     |x::x'::xs'=>card_color(x)=card_color(x') andalso all_same_color(x'::xs')


fun sum_cards cs =
  case cs of
       []=>0
     |x::xs => card_value(x)+sum_cards(xs)

fun score(cs,goal)=
let val sum_=sum_cards cs
  val pre_=if sum<=goal then (goal-sum) else 3*(sum-goal)    
in
  if all_same_color cs then pre_ div 2 else pre_
end

fun officiate(cs,ms,goal)=
let 
  fun start(cards,moves,held_cards)=
    case moves of
         [] =>score(held_cards,goal)
       |m::ms=>case m of
                    Discard c => start(cards,ms,remove_card(held_cards,c,IllegalMove))
                  |Draw=> case cards of
                               []=>score(held_cards,goal)
                             |d::ds=>
                                 let val score_=score(d::held_cards,goal) in
                                   if  score_>goal then score_ else
                                     start(ds,ms,d::held_cards)
end
                                 in
                                   start(cs,ms,[])    
                                 end
