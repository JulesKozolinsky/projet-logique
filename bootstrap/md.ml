(******************************************************************************)
(*                                                                            *)
(*                      INVERSION DE MD* VIA SAT SOLVEUR                      *)
(*                                                                            *)
(*                      Projet Logique 2016 - Partie SAT                      *)
(*   Auteur, license, etc.                                                    *)
(******************************************************************************)

(* Input: only one chunk of 512 bits (16 words of 32 bits), NO padding
   Output: digest of 128 bits (WITHOUT padding)
By default:
- 4 rounds for the only chunk
- each round is made of 16 steps
- each step consume a word of the input (not linear !)
- can be seen as 64 steps such that non-linear functions (i.e., F,G,H,I), and
  parts of inputs that are consumed depend on the current step
 *)

open Param
open Data
open Printf

let xor32 a b = 
  let res = Array.make 32 false in
  for k = 0 to 31 do
    res.(k) <- ((a.(k)&&(not b.(k))) || (b.(k)&&(not a.(k)))) ;
  done ;
res ;;
(*
let addBitABit a b = 
  let res = Array.make32 false in 
  let retenue = false in
  for k = 0 to 31 do 
    res.(k) <- 
*)
let k = (let pre_k = Array.make 64 0 in
	 for i = 0 to 63 do
	   pre_k.(i) <- abs(int_of_float(sin(float_of_int(i + 1))) mod  4294967296) ; (* valeur de 2^32 *)
	 done ;
	 pre_k);;
	     


(*** Main function ***)	  
let compute input = (* Array.make 128 false  [TODO] *)
  let w = vectK in 
  let r = vectS in
  let a = a0 in
  let b = b0 in 
  let c = c0 in
  let d = d0 in 
(*
  for i = 0 to 63 do 
    (if i < 16 then 
      let f = (b&&c) || ((not b)&&d) in
      let g = i in
    else if i < 32 then
      let f = (d&&b) || ((not d)&&c) in
      let g = (5*i + 1) mod 16 in
    else if i < 48 then
      let f = xor32 (xor32 b c) d in
      let g = (3*1 + 5) mod 16 in
    else 
      let f = xor32 c (b || (not d)) in
      let g = 7*i mod 16 in ) ; 
    let tempInt = d in
    let d = c in
    let c = b in
    let b = ((a + 
*)


        


  Array.make 128 false
