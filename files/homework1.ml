(* 

HOMEWORK 1

Due: Wed Sep 11, 2019 (23h59)

Name: Siddharth Garimella

Email: sid@students.olin.edu

Remarks, if any: fun assignment

*)


(*
 *
 * Please fill in this file with your solutions and submit it
 *
 * The functions below are stubs that you should replace with your
 * own implementation.
 *
 * PLEASE DO NOT CHANGE THE TYPES IN THE STUBS BELOW.
 * Doing so risks making it impossible for me to test your code.
 *
 * Always make sure you can #use this file in a FRESH OCaml shell
 * before submitting it. It has to load without any errors.
 *
 *)



(* Question 1 *)


let rec gcd (a:int) (b:int):int =
  if a = b then a else if a > b then gcd (a-b) b else gcd a (b-a);;

let rec coprime (a:int) (b:int):bool =
  if (gcd a b) = 1 then true else false 
  
let rec euler (n:int):int =
  let rec count co m =
    if m < n then (count (if coprime m n then co + 1 else co) (m+1)) else co
    in
    if n = 1 then 1 else count 0 1

let rec collatz (a:int):int list =
  if a=1 then [1] else a::collatz((if a mod 2 = 0 then a/2 else 3*a+1))

  
let rec range (i:int) (j:int):int list =
  if i = j-1 then [i] else i::range (i+1) (j)
  

(* Question 2 *)

let rec squares (xs:int list):int list = match xs with
    | [] -> []
    | x::xs' -> (x*x)::squares(xs')

  
let rec concatAll (xs: 'a list list): 'a list = match xs with
    | [] -> []
    | x::xs' -> x@concatAll(xs')

  
let rec doubleAndOne (xs:int list):int list = match xs with
    | [] -> []
    | x::xs' -> x::x+1::doubleAndOne(xs')

  
let rec nonZero (xs:int list):int list = match xs with
    | [] -> []
    | x::xs' -> if x=0 then nonZero xs' else x::nonZero xs'

  
let rec classify (xs:int list):int list * int list = match xs with
    | [] -> ([], [])
    | x::xs' -> match classify(xs') with 
    | (a,b) -> if x>=0 then (x::a, b) else (a, x::b)
  

(* QUESTION 3 *)

let rec scale (a:int) (v:int list):int list = match v with
    | [] -> []
    | x::v' -> (a*x)::scale a v'


let rec add (v:int list) (w:int list):int list = match v with
    | [] -> []
    | x::xs' -> (match w with
               | [] -> []
               | y::ys' -> x+y::add xs' ys')


let rec length (v:int list): float = match v with
    | [] -> 0.
    | x -> let rec sumsq m = match m with
                            | [] -> 0.
                            | ms::m' -> (float ms ** 2.) +. sumsq m'
            in
            sqrt (sumsq x)

  
let rec inner (v:int list) (w:int list):int = match v with
    | [] -> 0
    | x::xs' -> (match w with
               | [] -> 0
               | y::ys' -> x*y + inner xs' ys')
        
           
let rec outer (v:int list) (w:int list):int list list = match v with
    | [] -> []
    | x::xs' -> (match w with
               | [] -> []
               | y -> (scale x y)::outer xs' y)
