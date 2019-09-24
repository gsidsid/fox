(* 

HOMEWORK 3

Due: Fri Sep 27, 2019 (23h59)

Name: 

Email:

Remarks, if any:

*)


(* 
 * String <-> characters utility functions:
 *
 *   explode : string -> char list
 *      returns the list of characters making up a string
 *
 *   implode : char list -> string
 *      concatenates the list of characters into a string
 *
 *)

let explode (str) = 
  let rec acc (index,result) = 
    if (index<0) then
      result
    else
      acc(index-1, (String.get str index)::result)
  in
    acc(String.length(str)-1, [])

let implode cs = 
  List.fold_right (fun a r -> (String.make 1 a)^r) cs ""



(*
 *  The type of a finite automaton
 * 
 *)

type 'a fa = { states: 'a list;
               alphabet: char list;
               delta: ('a * char * 'a) list;
               start : 'a;
               final : 'a list }


(* 
 * Sample FAs
 *
 * The first accepts the language of all strings over {x,y} 
 * with a multiple-of-3 number of x's.
 *
 * The second accepts the language of all strings over {x,y,z} 
 * whose last three symbols are y's.
 *
 * Notes that states can be of any type -- the first example
 * uses strings as states, while the second uses integers.
 *
 *)

let faThreeX = { 
  states = ["start";"one";"two"];
  alphabet = ['x';'y'];
  delta = [ ("start",'x',"one");
	    ("one",'x',"two");
	    ("two",'x',"start");
	    ("start",'y',"start");
	    ("one",'y',"one");
	    ("two",'y',"two") ];
  start = "start";
  final = ["start"]
} 

let faLastThreeY = {
  states = [0;1;2;3];
  alphabet = ['x';'y';'z'];
  delta = [ (0,'x',0);
	    (0,'y',0);
	    (0,'z',0);
	    (0,'y',1);
	    (1,'y',2);
	    (2,'y',3); ];
  start = 0;
  final = [3]
} 





(* QUESTION 1 *)


let rec hasFinal (m:'a fa) (qs:'a list):bool =
  failwith "Not implemented"

  
let rec reachableStates (m:'a fa) (q:'a) (a:char):'a list = 
  failwith "Not implemented"

  
let rec follow (m:'a fa) (qs:'a list) (a:char):'a list = 
  failwith "Not implemented"

  
let rec followAll (m:'a fa) (qs:'a list) (syms:char list):'a list = 
  failwith "Not implemented"

  
let accept (m:'a fa) (input:string):bool = 
  failwith "Not implemented"



(* QUESTION 2 *)

(* Right now, these are dummy finite automata that always rejects 
   replace by your own *)

let dummy = { states = [[]];
              alphabet = [];
	      delta = [];
	      start = [];
	      final = []}

          
let fa_q2_a : 'a fa = dummy
                    

let fa_q2_b : 'a fa = dummy
                        

let fa_q2_c : 'a fa = dummy
                    

let fa_q2_d : 'a fa = dummy


let fa_q2_e : 'a fa = dummy
    

                    
(* QUESTION 3 *)

let fa1 = { states = ["fa1_1";"fa1_2";"fa1_3"];
	    alphabet = ['x';'y';'z'];
	    delta = [ ("fa1_1",'x',"fa1_2");
		      ("fa1_2",'x',"fa1_3");
		      ("fa1_3",'x',"fa1_1") ];
	    start = "fa1_1";
	    final = ["fa1_1"] }

let fa2 = { states = ["fa2_1";"fa2_2";"fa2_3"];
	    alphabet = ['x';'y';'z'];
	    delta = [ ("fa2_1",'y',"fa2_2");
		      ("fa2_2",'z',"fa2_3");
                      ("fa2_3",'y',"fa2_2") ];
	    start = "fa2_1";
	    final = ["fa2_3"] }


let unionM (m1:string fa) (m2:string fa):string fa = 
  (* you can assume same alphabet *)
  (* you can assume the states are different *)
  (* you can assume string states *)
  failwith "Not implemented"


let concatM (m1:string fa) (m2:string fa):string fa = 
  (* you can assume the same alphabet *)
  (* you can assume the states are different *)
  (* you can assume string states *)
  failwith "Not implemented"

   
let reverseM (m1:string fa):string fa =
  (* you can assume string states *)
  failwith "Not implemented"


  
  
(* This function is the base function that basically loops through all
 * strings  
 * of length up to n, and prints those that are accepted by the
 * finite automaton.
 *
 * This is being way too clever to try to not blow the stack 
 * while enumerating all strings up to a given length. Basically.
 * we enumerate all integer, convert them to base K (where K is the
 * size of the alphabet) and then replace every digit base K by the
 * letter of the alphabet at the corresponding index in the alphabet. 
 *
 * The key is that we can enumerate integers super easily
 *
 *)

let lang m n = 

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
  	  let ts = to_string m.alphabet i  in
  	  let bound = expt (List.length m.alphabet) i in
  	  let rec loop2 j = 
  	    if j < bound then (if accept m (ts j) 
                                 then print_str (ts j)
                               else ();
  			       loop2 (j+1))
  	    else ()  in
  	  (loop2 0; loop (i+1))
        else ()  in
      loop 0

