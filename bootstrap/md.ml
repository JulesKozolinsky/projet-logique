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

let et32 a b = 
  let res = Array.make 32 false in
  for i = 0 to 31 do
    res.(i) <- a.(i) && b.(i) ;
  done ;
  res ;;

let ou32 a b = 
  let res = Array.make 32 false in
  for i = 0 to 31 do
    res.(i) <- a.(i) || b.(i) ;
  done ;
  res ;;

let not32 a = 
  let res = Array.make 32 false in
  for i = 0 to 31 do
    res.(i) <- not a.(i) ;
  done ;
  res ;;

let leftrotate tab int = 
  let res = Array.make 32 false in
  for i = 0 to 31 do 
    res.(i) <- tab.(i - int + 32 mod 32) ;
  done ;
  res ;;

let equivbool b1 b2 = (b1&&b2) || ((not b1) && (not b2)) ;;

(* fait une addition bit à bit de deux tableaux de booléens de taille 32 avec retenues *)
let addBitABit a b = 
  let res = Array.make 32 false in 
  let retenue = ref false in
  for i = 0 to 31 do
    res.(i) <- if (equivbool a.(i) b.(i)) then !retenue else (not !retenue) ;
    retenue := (!retenue && (a.(i) || b.(i))) || (a.(i) && b.(i)) ;
  done ;
  res ;;

let slice32 tab = 
  let res = Array.make 32 (Array.make 32 false) in
  for k = 0 to 15 do 
    res.(k) <- Array.sub tab (32*k) (32*(k+1)) ;
  done ;
  res ;;

(*** Main function ***)	  
let compute input = (* input est le w de wikipédia ; le w de l'archive *)
  let k = vectK in (* le k de wikipédia ; le t de l'archive *) (* c'est un bool array array : 16 tableaux de taille 32 *)
  let r = vectS in (* le r de wikipédia ; le r de l'archive *) (* c'est un int array de taille 64 *)
  let a = ref a0 in (* S_(i-4) *)
  let b = ref b0 in (* S_(i-1) *)
  let c = ref c0 in (* S_(i-2) *)
  let d = ref d0 in (* S_(i-3) *)
  (* ce sont 4 tableaux de booléens de taille 32 *)

  let new_input = slice32 (input) in

  let f = ref !a in
  let g = ref 0 in 

  for i = 0 to 63 do 
    (if i < 16 then (
      f := ou32 (et32 !b !c) (et32 (not32 !b) !d) ;
      g := i ;
     )
     else (if i < 32 then (
       f := ou32 (et32 !d !b) (et32 (not32 !d) !c) ;
       g := (5*i + 1) mod 16 ;
     )
       else (if i < 48 then (
	 f := xor32 (xor32 !b !c) !d ;
	 g := (3*i + 5) mod 16 ;
       )
	 else (
	   f := xor32 !c (ou32 !b (not32 !d)) ;
	   g := (7*i) mod 16 ; 
	 )
       )));
    let tempInt = !d in
    d := !c ;
    c := !b ;
    b := addBitABit (leftrotate (addBitABit (addBitABit (addBitABit !a !f) new_input.(!g)) k.(i)) r.(i)) !b ;
    a := tempInt ;
  done ;
  


  Array.make 128 false
