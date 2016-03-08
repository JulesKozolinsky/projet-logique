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
          | _ -> failwith "Satisfaire le compilateur" );
          formula_list := (And (Or(Or (Lit(!var_c),Lit(!var_a)),
                                   Or (Lit(!var_b),Lit(!var_c'))),
                                Or(Or (Lit(!var_c),Lit(!var_a)),
                                   Or (Lit(!var_b),Lit(!var_s)))) )
            ::(!formula_list) 
      done;
    done;
  done;
;;

let int_to_bool x = match x with 
  | _ when x = 0 -> false 
  | _ when x = 1 -> true 
  | _ -> failwith "problème int_to_bool"
;;

let bool_to_int b = match b with 
  | _ when b -> 1 
  | _ when (not b) -> 0 
  | _ -> failwith "problème bool_to_int"

let rec count_int_list l = match l with 
  | [] -> 0 
  | a::suite -> a + count_int_list suite
;;


let compute_add c1_int c2_int a_int b_int c_int d_int = 
  let res = count_int_list [c1_int;c2_int;a_int;b_int;c_int;d_int]
  in 
  let res_liste = Array.make 3 0 in
  for i = 0 to 2 do
    res_liste.(i) <- (res/(Data.pow 2 i)) mod 2 
  done;
  res_liste
;;

let int_to_litt_arg x = function 
  | 0 -> Pos(x)
  | 1 -> Neg(x)
  | _ -> failwith "pb int_to_litt_arg"
;;

let int_to_litt_res x = function 
  | 0 -> Neg(x)
  | 1 -> Pos(x) 
  | _ -> failwith "pb int_to_litt_res"
;;

let add4 c1 c2 a b c d_bool s c1' c2' formula_list = 
  let var_c1 = ref (Pos(c1)) in 
  let var_c2 = ref (Pos(c2)) in 
  let var_a = ref (Pos(a)) in 
  let var_b = ref (Pos(b)) in 
  let var_c = ref (Pos(c)) in
  let var_s = ref (Pos(s)) in
  let var_c1' = ref (Pos(c1')) in 
  let var_c2' = ref (Pos(c2')) in
  let var_d = Const(d_bool) in
  for i = 0 to 1 do
    for j = 0 to 1 do
      for k = 0 to 1 do 
        for l = 0 to 1 do
          for m = 0 to 1 do 
            let res_liste = compute_add i j k l m (bool_to_int d_bool) in
            var_a := int_to_litt_arg a i ; 
            var_b := int_to_litt_arg b j ;
            var_c := int_to_litt_arg c k ;
            var_c1 := int_to_litt_arg c1 l ; 
            var_c2 := int_to_litt_arg c2 m ; 
            var_s := int_to_litt_res s (res_liste.(0)) ; 
            var_c1' := int_to_litt_res c1' (res_liste.(1)) ; 
            var_c2' := int_to_litt_res c2' (res_liste.(2)) ; 
            formula_list := Or(Or(Or(!var_a,!var_b),
                                  Or(!var_c,!var_c1)),
                               Or(Or(!var_c2,!var_s),
                                  Or(var_b,Const(false))))::!formula_list;
            formula_list := Or(Or(Or(!var_a,!var_b),
                                  Or(!var_c,!var_c1)),
                               Or(Or(!var_c2,!var_c1'),
                                  Or(var_b,Const(false))))::!formula_list;
            formula_list := Or(Or(Or(!var_a,!var_b),
                                  Or(!var_c,!var_c1)),
                               Or(Or(!var_c2,!var_c2'),
                                  Or(var_b,Const(false))))::!formula_list;

          done;
        done;
      done;
    done;
  done;
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
  for i = 0 to (!steps * !rounds -1 ) do
    if i < (!steps) && (!rounds>=1) then (
      f := ou32 (et32 !b !c) (et32 (not32 !b) !d) ;
      g := i ;
    )
    else (if i < (2 * !steps) && (!rounds>=2) then (
            f := ou32 (et32 !d !b) (et32 (not32 !d) !c) ;
            g := (5*i + 1) mod 16 ;
         )
          else (if i < (3 * !steps) && (!rounds>=3) then (
                  f := xor32 (xor32 !b !c) !d ;
                  g := (3*i + 5) mod 16 
                )
                else (if i < (4 * !steps) && (!rounds>=4) then (
                        f := xor32 !c (ou32 !b (not32 !d)) ;
                        g := (7*i) mod 16 )
                      )
    ));
  ()
    (* 1ère étape S_i <- S_(i-4) + f(S_(i-1),S_(i-2),S_(i-3)) + W[j] + T[i] *)
    
  (* TODO *)


  done;


  Formula.formulaeToCnf(!formula_list)
;;
