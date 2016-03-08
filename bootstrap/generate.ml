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

(* Crée les formules logiques pour l'addition a et b (et c) de retenue c' et de
 * résultat s *)
let add2 c a b c' s formula_list = 
  let var_c = ref (Pos(c)) in 
  let var_a = ref (Pos(a)) in 
  let var_b = ref (Pos(b)) in 
  let var_c' = ref (Pos(c')) in 
  let var_s = ref (Pos(s)) in 
  for i = 0 to 1 do 
    for j = 0 to 1 do
      for k = 0 to 1 do 
        (match (i,j,k) with
          | (0,0,0) ->( var_c  := Pos(c) ; 
                        var_a  := Pos(a) ; 
                        var_b  := Pos(b) ;
                        var_c' := Neg(c') ; 
                        var_s  := Neg(s) )

          | (0,0,1) ->( var_c  := Pos(c) ;
                        var_a  := Pos(a) ;
                        var_b  := Neg(b) ;
                        var_c' := Neg(c') ; 
                        var_s  := Pos(s) )

          | (0,1,0) ->( var_c  := Pos(c) ;
                        var_a  := Neg(a) ;
                        var_b  := Pos(b) ;
                        var_c' := Neg(c') ;
                        var_s  := Pos(s) )

          | (0,1,1) ->( var_c  := Pos(c) ;
                        var_a  := Neg(a) ; 
                        var_b  := Neg(b) ;
                        var_c' := Pos(c') ;
                        var_s  := Neg(s) )

          | (1,0,0) ->( var_c  := Neg(c) ;
                        var_a  := Pos(a) ;
                        var_b  := Pos(b) ;
                        var_c' := Neg(c') ;
                        var_s  := Pos(s) )

          | (1,0,1) ->( var_c  := Neg(c) ; 
                        var_a  := Pos(a) ; 
                        var_b  := Neg(b) ;
                        var_c' := Pos(c') ; 
                        var_s  := Neg(s) )

          | (1,1,0) ->( var_c  := Neg(c) ;
                        var_a  := Neg(a) ; 
                        var_b  := Pos(b) ;
                        var_c' := Pos(c') ;
                        var_s  := Neg(s) )

          | (1,1,1) ->( var_c  := Neg(c) ; 
                        var_a  := Neg(a) ; 
                        var_b  := Neg(b) ;
                        var_c' := Pos(c') ;
                        var_s  := Pos(s) ) 
          | _ -> failwith "Satisfaire le compilateur" )
      done;
    done;
  done;
  formula_list := (And (Or(Or (Lit(!var_c),Lit(!var_a)),
                           Or (Lit(!var_b),Lit(!var_c'))),
                        Or(Or (Lit(!var_c),Lit(!var_a)),
                           Or (Lit(!var_b),Lit(!var_s)))) )::(!formula_list) 

;;

let add4 c1 c2 a b c d s formula_list = 
  let var_c1 = ref (Pos(c1)) in 
  let var_c2 = ref (Pos(c2)) in 
  let var_a = ref (Pos(a)) in 
  let var_b = ref (Pos(b)) in 
  let var_c = ref (Pos(c)) in 
  let var_d = ref (Pos(d)) in 
  let var_s = ref (Pos(s)) in 
  for i = 0 to 1 do
    for j = 0 to 1 do
      for k = 0 to 1 do 
        for l = 0 to 1 do
          for m = 0 to 1 do 
            for n = 0 to 1 do
              (match (i,j,k,l,m,n) with 
              | (0,0,0,0,0,0) -> var_c1 := Pos(c1)
              | _ -> failwith "Satisfaire le compilateur") 
            done;
          done;
        done;
      done;
    done;
  done;


  ()
;;


(*** Main function ***)
let genCNF digest =
	let d = digest in 
  let formula_list = ref [] in 

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

  (* On écrit les formules logiques *)
	

	 Formula.formulaeToCnf([])
;;





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
				(* TODO *)
