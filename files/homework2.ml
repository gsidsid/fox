(* 

HOMEWORK 2

Due: Wed Sep 18, 2019 (23h59)

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


(* Parse a string into a regular expression according to the grammar:
 *  
 *   R ::= R1 + R
 *         R1
 *   
 *   R1 ::= R2 R1
 *          R2
 *   
 *   R2 ::= R3*
 *          R3
 *   
 *   R3 ::= a
 *          1
 *          0 
 *          ( R )
 *)
          
type re = 
  | Empty 
  | Unit 
  | Letter of string 
  | Plus of re * re 
  | Times of re * re 
  | Star of re

let parse (s:string):re =
  let fromChar c = String.make 1 c in
  let explode s = 
    let rec loop i result = 
      if i < 0
      then result
      else loop (i-1) (s.[i]::result) in
    loop (String.length s - 1) []  in
  let isalpha = function 'A'..'Z'|'a'..'z' -> true | _ -> false in
  let expect c cs = 
    match cs with 
    | f::cs when f = c -> Some cs
    | _ -> None in
  let expect_alpha cs = 
    match cs with
    | f::cs when isalpha f -> Some (f,cs)
    | _ -> None  in
  let rec parse_R cs = 
    match parse_R1 cs with
    | None -> None
    | Some (r1,cs) -> 
       (match expect '+' cs with
        | None -> Some (r1,cs)
        | Some cs -> 
           (match parse_R cs with
            | None -> None
            | Some (r2,cs) -> Some (Plus(r1,r2),cs)))
  and parse_R1 cs = 
    match parse_R2 cs with
    | None -> None
    | Some (r1,cs) -> 
       (match parse_R1 cs with
        | None -> Some (r1,cs)
        | Some (r2,cs) -> Some (Times(r1,r2),cs))  
  and parse_R2 cs = 
    match parse_R3 cs with
    | None -> None
    | Some (r1,cs) -> 
       (match expect '*' cs with
        | None -> Some (r1,cs)
        | Some cs -> Some (Star(r1),cs))
  and parse_R3 cs = 
    match expect_alpha cs with
    | Some (a,cs) -> Some (Letter(fromChar(a)),cs)
    | None -> 
       (match expect '1' cs with
        | Some cs -> Some (Unit, cs)
        | None -> 
           (match expect '0' cs with
            | Some cs -> Some (Empty,cs)
            | None -> parse_parens cs))
  and parse_parens cs = 
    match expect '(' cs with
    | None -> None
    | Some cs -> 
       (match parse_R cs with
        | None -> None
        | Some (r,cs) -> 
           (match expect ')' cs with
            | None -> None
            | Some cs -> Some (r,cs)))  in
  let cs = explode s in
  match parse_R cs with
  | Some (re,[]) -> re
  | _ -> failwith ("Cannot parse "^s)

  
(* Function to check if a string is in the language
   described by a regular expression *)

let string_in_regexp (str:string) (re:string):bool =
  let explode s = 
    let rec loop i result = 
      if i < 0 then result
      else loop (i-1) (s.[i]::result) in
    loop (String.length s - 1) []  in
  let mkTimes = function
    | (Empty,_) -> Empty
    | (_,Empty) -> Empty
    | (Unit,r) -> r
    | (r,Unit) -> r
    | (r1,r2) -> Times(r1,r2)   in
  let mkPlus = function
    | (Empty,r) -> r
    | (r,Empty) -> r
    | (Unit,Unit) -> Unit
    | (r1,r2) -> Plus(r1,r2)   in
  let rec delta = function
    | Empty -> Empty
    | Unit -> Unit
    | Letter _ -> Empty
    | Plus (r1,r2) -> mkPlus(delta(r1),delta(r2))
    | Times (r1,r2) -> mkTimes(delta(r1),delta(r2))
    | Star (r) -> Unit  in
  let rec deriv c = function
    | Empty -> Empty
    | Unit -> Empty
    | Letter x when x.[0] = c -> Unit
    | Letter x -> Empty
    | Plus (r1,r2) -> mkPlus(deriv c r1,
  			 deriv c r2)
    | Times (r1,r2) -> mkPlus(mkTimes(delta r1,
  				deriv c r2),
  			  mkTimes(deriv c r1,
  				r2))
    | Star (r) -> mkTimes(deriv c r,
  		        Star(r))  in
  let rec m re = function 
    | [] -> (match delta re with Unit -> true | _ -> false)
    | c::cs -> m (deriv c re) cs  in
  m (parse re) (explode str)

  
(* This function is the base function that basically loops through all
 * strings of length up to n, and prints those that are in the
 * language of the regular expression
 *
 * This is being way too clever to try to not blow the stack 
 * while enumerating all strings up to a given length. Basically.
 * we enumerate all integer, convert them to base K (where K is the
 * size of the alphabet) and then replace every digit base K by the
 * letter of the alphabet at the corresponding index in the alphabet. 
 *
 * The key is that we can enumerate integers super easily
 *)

let lang r alphabet n = 
  let implode cs = 
    List.fold_right (fun a r -> (String.make 1 a)^r) cs "" in
  let rec expt a n = if n <= 0 then 1 else a*(expt a (n-1)) in
  let rec take n default l = 
    if n <= 0 then []
    else (match l with
          | [] -> default::(take (n-1) default l)
          | x::xs -> x::(take (n-1) default xs)) in
  let to_base_n base size n = 
    let rec loop n = 
      if n <= 0 then []
      else if n mod base = 0 then 0::(loop (n / base))
      else (n mod base)::(loop ((n - n mod base) / base))  in
    take size 0 (loop n)  in
  let to_string alphabet size n = 
    let base = List.length alphabet in
    let num_base = to_base_n base size n in
    implode (List.map (fun i -> List.nth alphabet i) num_base) in
    if n < 0 then ()
    else
      let print_str s = if s = "" then print_string "  <epsilon>\n"
  	                else print_string ("  "^s^"\n")  in
      let rec loop i = 
        if i <= n then 
  	  let ts = to_string alphabet i  in
  	  let bound = expt (List.length alphabet) i in
  	  let rec loop2 j = 
  	    if j < bound then (if string_in_regexp (ts j) r
                               then print_str (ts j)
                               else ();
  			       loop2 (j+1))
  	    else ()  in
  	  (loop2 0; loop (i+1))
        else ()  in
      loop 0

              

(************************************************************)

(* Q1 : set operations *)
  

let rec set xs = match xs with
    | [] -> []
    | x::xs' -> let rec isin cy ys = match ys with
        | [] -> false
        | y::ys' -> if cy = y then true else isin cy ys'
    in
    if isin x xs' then set xs' else x::set(xs');;

let rec set_sub xs ys = match xs with
   | [] -> true
   | x::xs' -> let rec isin cz zs = match zs with 
        | [] -> false
        | z::zs'-> if cz = z then true else isin cz zs'
    in
    if isin x ys then set_sub xs' ys else false;;

  
let rec set_eq xs ys = 
  set_sub xs ys && set_sub ys xs;;

  
let rec set_union xs ys = 
  set (xs@ys);;
  
  
let rec set_inter xs ys = match xs with
   | [] -> []
   | x::xs' -> let rec isin cz zs = match zs with 
        | [] -> false
        | z::zs'-> if cz = z then true else isin cz zs'
    in
    if isin x ys then x::(set_inter xs' ys) else set_inter xs' ys;;


(* Q2 : language operations *)
            
let rec prepend_string_to_lang x ys = match ys with
    | [] -> []
    | y::ys' -> (x^y)::prepend_string_to_lang x ys';;

  
let rec lang_concat xs ys = match xs with
    | [] -> []
    | x::xs' -> (prepend_string_to_lang x ys)@(lang_concat xs' ys);;
  

let rec lang_power n xs = 
    if n=0 then [""] else  
    let rec expo n ys zs = if n=1 then ys else expo (n-1) (lang_concat ys zs) (zs)
    in
    expo n xs xs
  

let rec lang_nstar n xs =   
    if n=0 then [""] else
    let rec expo n ys zs = if n=1 then ys else ys@(expo (n-1) (lang_concat ys zs) (zs))
    in
    [""]@(expo n xs xs);;

   
(* Q3 : regular expressions *)

   
let regexp_a = "(x+y+z)(x+y+z)(x+y+z)";;

let regexp_b = "((x+y+z)(x+y+z)(x+y+z))*"

let regexp_c = "((x+y+z)+(x+y+z)(x+y+z))((x+y+z)(x+y+z)(x+y+z))*"

let regexp_d = "(z*)(x)(z*)(x)(z*)"

let regexp_e = "(z*)(y)(z*)(x)(z*)(x)(z*)+(z*)(x)(z*)(y)(z*)(x)(z*)+(z*)(x)(z*)(x)(z*)(y)(z*)"
