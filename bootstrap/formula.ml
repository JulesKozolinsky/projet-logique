(******************************************************************************)
(*                                                                            *)
(*                      INVERSION DE MD* VIA SAT SOLVEUR                      *)
(*                                                                            *)
(*                      Projet Logique 2016 - Partie SAT                      *)
(*   Auteur, license, etc.                                                    *)
(******************************************************************************)

open Printf
open Param
       
type var = int
type literal =
  | Pos of var
  | Neg of var
type clause = literal list
type cnf = clause list

type formula =
  | Const of bool
  | Lit of literal
  | Not of formula
  | And of formula*formula
  | Or of formula*formula
  | Xor of formula*formula
  | Imply of formula*formula
  | Equiv of formula*formula

let rec displayFormula = function
  | Const b -> sprintf "%b" b
  | Lit l -> (match l with
	      | Pos d -> sprintf "+%d" d
	      | Neg d -> sprintf "-%d" d)
  | Not f -> sprintf "Not[%s]" (displayFormula f)
  | And (f1,f2) -> sprintf "[%s] /\\ [%s]" (displayFormula f1) (displayFormula f2)
  | Or (f1,f2) -> sprintf "(%s) \\/ (%s)" (displayFormula f1) (displayFormula f2)
  | Xor (f1,f2) -> sprintf "(%s) + (%s)" (displayFormula f1) (displayFormula f2)
  | Imply (f1, f2) -> sprintf "{%s} ==> {%s}" (displayFormula f1) (displayFormula f2)
  | Equiv (f1,f2) -> sprintf "{%s} <==> {%s}" (displayFormula f1) (displayFormula f2)
		
(* Prends une formule f et renvoie une formule composé de littéraux, de Ou et de Et *)	     
let rec simple  f = match f with 
  | Const b -> f
  | Lit l -> f
  | Not g -> (match g with  
    	| Const b -> Const (b = false)
	    | Lit l -> (match l with 
		    | Pos d -> Lit (Neg d) 
		    | Neg d -> Lit (Pos d) )
	    | Not h -> f
	    | And (f1,f2) -> simple(Or ((Not f1),(Not f2)))
	    | Or (f1,f2) -> simple(And ((Not f1),(Not f2)))
	    | Xor (f1,f2) -> simple (Or (And ((Not f1),(Not f2)), And (f1,f2)))
	    | Imply (f1,f2) -> simple (And (f1,(Not f2)))
	    | Equiv (f1,f2) -> simple (Or (Not (Equiv (f1,f2)),Not (Equiv (f1,f2))) ) )
  | And (f1,f2) -> (match (f1,f2) with 
      | (Const true,f2) -> simple (f2)
      | (Const false,f2) -> Const false
      | (f1, Const true) -> simple (f1)
      | (f1, Const false) -> Const false
      | (f1,f2) -> And (simple f1, simple f2) )
  | Or (f1,f2) -> (match (f1,f2) with 
      | (Const true, f2) -> Const true 
      | (Const false, f2) -> simple (f2)
      | (f1, Const true) -> Const true
      | (f1, Const false) -> simple (f1)
      | (f1,f2) -> Or (simple f1,simple f2))
  | Xor (f1,f2) -> simple (Or (And (f1,(Not f2)),And ((Not f1),f2) )) 
  | Imply (f1, f2) -> simple(Or ((Not f1),f2)) 
  | Equiv (f1,f2) -> simple (Or ((And (f1, f2)),(And ((Not f1),(Not f2))) ))  
;;

(* Si l appartient à tau, on stocke son évluation dans s et on renvoie vrai,
  sinon on renvoie faux *)
let rec appartient l tau s = match tau with 
  | [] -> false
  | (x,b)::_ when x = l -> ( s := b ; true ) 
  | _::suite -> appartient l suite s 
;;


let rec subst_aux f tau = match f with
  | Lit l -> ( let s = ref false and n = ref false and var = ref 0 in
		(match l with 
		 | Pos d -> ( var := d ; n := true)
		 | Neg d -> ( var := d ; n := false));
             if appartient !var tau s 
             then (if !n then Const !s else Not (Const !s))
             else Lit l )
  | And (f1,f2) -> And (subst_aux f1 tau, subst_aux f2 tau)
  | Or (f1,f2) -> Or (subst_aux f1 tau, subst_aux f2 tau)
  | _ -> failwith "on ne tombe pas dans ce cas" 
;;

let subst f tau = simple (subst_aux (simple f) tau);;

let rec ftc f = match f with 
  | Lit l -> f 
  | And (f1,f2) -> And(ftc f1,ftc f2) (* la composition de deux CNF est une CNF *) 
  | Or (f1,f2) -> ( match ((ftc f1,ftc f2) with 
			| (And(g1,g2),d) ->  And(And(ftc Or(g1,d1),ftc Or(g1,d2)),And(ftc Or(g2,d1),ftc Or(g2,d2)))
			| (

;;

let formulaeToCnf fl = [[]]	(* [TODO] *)
	      
let displayCnf cnf = ""		(* [TODO] *)

(*** TEST ***)
let dummyCNF =
  [[Pos 1;Pos 2;Pos 3];
   [Pos 2;Pos 3;Pos 4];
   [Pos 3;Pos 4;Neg 1]
  ]
let sat_solver = ref "./minisat"

(** Return the result of minisat called on [cnf] **)
let testCNF cnf = 
  let cnf_display = displayCnf cnf in
  let fn_cnf = "temp.out" in
  let oc = open_out fn_cnf in
  Printf.fprintf oc  "%s\n" cnf_display;
  close_out oc;
  let resc = (Unix.open_process_in
		(!sat_solver ^ " \"" ^ (String.escaped fn_cnf)
		 ^ "\"") : in_channel) in
  let resSAT = let acc = ref [] in
	       try while true do
		     acc := (input_line resc) :: !acc
		   done; ""
	       with End_of_file ->
		 close_in resc;
		 if List.length !acc = 0
		 then begin
		     log ~level:High "It seems that there is no executable called 'minisat' at top level.";
		     exit 0;
		   end
		 else String.concat "\n" (List.rev !acc) in
  close_in resc;
  List.hd (List.rev (Str.split (Str.regexp " +") resSAT))
	   
let test () =
  ()			     (* [TODO] *)
  (* Yout test using your functions here *)
