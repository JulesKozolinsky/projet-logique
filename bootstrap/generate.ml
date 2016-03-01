(******************************************************************************)
(*                                                                            *)
(*                      INVERSION DE MD* VIA SAT SOLVEUR                      *)
(*                                                                            *)
(*                      Projet Logique 2016 - Partie SAT                      *)
(*   Auteur, license, etc.                                                    *)
(******************************************************************************)
		
open Param
open Printf
open Formula
open Data

(* Cela représente le numéro du litteral *)
let label = ref 0 ;;

(* Donne un numéro libre pour un litteral *)
let get_label () = 
	incr label ;
	!label
;;
	
	 
	

(*** Main function ***)
let genCNF digest =
	let d = digest in 

	(* On initialise le message m *) 
	let m = Array.make 512 0 in 
	for i = 0 to 511 do 
		m.(i) <- get_label() 
	done;
	
	(* On initialise le tableau s *) 
	let s = Array.make_matrix 64 3 [||] in 
	for i = 0 to 63 do 
		for j = 0 to 2 do
			let t = Array.make 32 0 in 
			for k = 0 to 31 do 
				t.(k) <- get_label() 
			done;
			s.(i).(j) <- t;
		done;
	done; 

	(* On initialise le tableau carries *)
	let c = Array.make_matrix 64 3 [||] in 
        for i = 0 to 63 do 
                for j = 0 to 2 do 
                        let t = Array.make 32 0 in 
                        for k = 0 to 31 do 
                                t.(k) <- get_label()
                        done;
                        c.(i).(j) <- t;
                done;
        done;
 
	

	Formula.formulaeToCnf([])	





    (*    let d = digest in
        let formula_list = ref [] in
        for i = 0 to 10 do                  
                formula_list := (Equiv ( Const d.(i),
                                        Xor (Xor (Const (d.(i+10)),
                                                  Lit(Pos( ((i*13) mod 512)+1 ))) ,
                                             Xor (Lit(Pos( ((i*14+1) mod 512)+1)) ,
                                                  Lit(Pos( ((i*15+2) mod 512)+1))) ))) ::(!formula_list);
        done;
        formula_list := (Or (Lit(Pos (512)),Lit(Neg (512))))::(!formula_list);
        Formula.formulaeToCnf(!formula_list) *)
;;
				(* TODO *)


