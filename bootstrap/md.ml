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
open Array

let xor32 a b = 
  let res = Array.make 32 false in
  for k = 0 to 31 do
    res.(k) <- ((a.(k)&&(not b.(k))) || (b.(k)&&(not a.(k)))) ;
  done ;
res ;;

let equivbool b1 b2 = (b1&&b2) || ((not b1) && (not b2)) ;;

let addBitABit a b = 
  let res = Array.make 32 false in 
  let retenue = ref false in
  for i = 0 to 31 do
    res.(i) <- if (equivbool a.(i) b.(i)) then !retenue else (not !retenue) ;
    retenue := (!retenue && (a.(i) || b.(i))) || (a.(i) && b.(i)) ;
  done ;
res ;;


(*** Main function ***)	  
let compute input = (* input est le w de wikipédia ; le t de l'archive *)
  let k = vectK in (* le k de wikipédia ; le w de l'archive *) (* c'est un bool array array : 16 tableaux de taille 32 *)
  let r = vectS in (* le r de wikipédia ; le r de l'archive *) (* c'est un int array de taille 64 *)
  let a = a0 in (* S_(i-4) *)
  let b = b0 in (* S_(i-1) *)
  let c = c0 in (* S_(i-2) *)
  let d = d0 in (* S_(i-3) *)
  (* ce sont 4 tableaux de booléens de taille 32 *)  
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
      let g = (3*i + 5) mod 16 in
    else 
      let f = xor32 c (b || (not d)) in
      let g = (7*i) mod 16 in ) ; 
    let tempInt = d in
    let d = c in
    let c = b in
    let b = ((addBitABit a addBitABit f addBitABit input.(i) k.(g)) leftrotate r.(i)) + b in
    let a = tempInt in
  done ;
    
*)

  Array.make 128 false
