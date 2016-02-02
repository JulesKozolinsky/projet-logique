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
		;;

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
  |And(p,q) -> (match (simple(p),simple(q)) with
	   |Const(true),sq -> sq
	   |Const(false),_ -> Const(false)
	   |sp,Const(true) -> sp
	   |_,Const(false) -> Const(false)
	   |sp,sq          -> And(sp,sq))
  |Or(p,q) -> (match (simple(p),simple(q)) with
	   |Const(true),_   -> Const(true)
	   |Const(false),sq -> sq 
	   |_,Const(true)   -> Const(true)
	   |sp,Const(false) -> sp
	   |sp,sq           -> Or(sp,sq))
  | Xor (f1,f2) -> simple (Or (And (f1,(Not f2)),And ((Not f1),f2) )) 
  | Imply (f1, f2) -> simple(Or ((Not f1),f2)) 
  | Equiv (f1,f2) -> simple (Or ((And (f1, f2)),(And ((Not f1),(Not f2))) ))  
;;



(**  SUBSTITUTION  **)

(* Si l appartient à tau, on stocke son évluation dans s et on renvoie vrai,
  sinon on renvoie faux *)
let rec appartient l tau s = match tau with 
  | [] -> false
  | (x,b)::_ when x = l -> ( s := b ; true ) 
  | _::suite -> appartient l suite s 
;;

let rec subst_aux f tau = match f with
  | Const _ -> f
  | Lit l -> ( let s = ref false in
		(match l with 
		 | Pos d -> if appartient d tau s then Const !s else Lit l
		 | Neg d -> if appartient d tau s then Not (Const !s) else Lit l ))
  | And (f1,f2) -> And (subst_aux f1 tau, subst_aux f2 tau)
  | Or (f1,f2) -> Or (subst_aux f1 tau, subst_aux f2 tau)
  | _ -> failwith "normalement on ne rentre pas dans ce cas là" 
;;

let subst f tau = simple (subst_aux (simple f) tau);;



(**  FORMULE -> CNF  **)

(* Prend une formule composée de littéraux de Et et de Ou et renvoie une formule sous forme normale conjonctive *)
let rec ftc f = match f with 
  | Const _ -> f
  | Lit l -> f 
  | And (f1,f2) -> And(ftc f1,ftc f2) (* la composition de deux CNF est une CNF *) 
  | Or (f1,f2) -> ( match (ftc f1,ftc f2) with 
			| (And(g1,g2),d) ->  ftc (And(Or(g1,d),Or(g2,d)))
			| (g,And(d1,d2)) ->  ftc (And(Or(g,d1),Or(g,d2)))
			| (g,d) -> Or(g,d) )
  | _ -> failwith "normalement on ne rentre pas dans ce cas là"
;;


let fusionInside l1 l2 = match l1,l2 with
  |[l1],[l2] -> [l1@l2]
  |_,_ -> failwith "on est foutus" ;;

(* Prend une formule sous forme normale conjonctive et renvoie une CNF *)
let rec formulaToCnf f = match f with
  |And(p,q) -> (formulaToCnf p)@(formulaToCnf q)
  |Or(p,q)  -> fusionInside (formulaToCnf p) (formulaToCnf q)
  |Lit(n)   -> [[n]]
  |_        -> failwith "formulaToCnf" ;;





let rec concat_et_applique f liste = match liste with 
	| [] -> [] 
	| a::suite -> (f a)@(concat_et_applique f suite)
;;

(* prend une liste de formules sous forme nc et renvoie la CNF correspondant à la conjonction de ces formules *)
let formulaeToCnf fl = concat_et_applique (  fun x -> formulaToCnf(ftc(simple x)) ) fl
;;


(**  AFFICHAGE  **)

let abs n = if n>=0 then n else -n ;;

let display_lit lit = match lit with 
  |Pos(n) -> sprintf "%d" n
  |Neg(n) -> sprintf "-%d" n 
;;

let rec display_clause clause = match clause with
  |[] -> sprintf "0"
  |t::q -> sprintf "%s %s" (display_lit t) (display_clause q)
;;

let rec display_conj conj = match conj with
  |[] -> sprintf "\n"
  |t::q -> sprintf "%s \n%s" (display_clause t) (display_conj q)
;;

let rec liste_lit_in_clause clause liste_lit = match clause with
  |[] -> liste_lit
  |Pos(n)::q -> if List.mem (abs n) liste_lit then liste_lit_in_clause q liste_lit else liste_lit_in_clause q ((abs n)::liste_lit)
  |Neg(n)::q -> if List.mem (abs n) liste_lit then liste_lit_in_clause q liste_lit else liste_lit_in_clause q ((abs n)::liste_lit)
;;

let rec liste_lit_in_conj conj liste_lit = match conj with
  |[] -> liste_lit
  |t::q -> liste_lit_in_conj q (liste_lit_in_clause t liste_lit)
;;

let displayCnf cnf = 
sprintf "p cnf %d %d \n%s" (List.length (liste_lit_in_conj cnf [])) (List.length cnf) (display_conj cnf)
;;



let test =  And(And(Lit(Neg 5),Lit(Pos 4)),Xor(Lit (Pos 2),Lit (Pos 3)));;

(* formulaeToCnf [test;test];; *)
print_string (displayFormula test);;
print_newline () ;;
print_string (displayCnf(formulaeToCnf [test]));;
print_newline () ;;


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
;;	   

let test () =
  let exn  =  And(And(Lit(Neg 1),Lit(Pos 4)),Xor(Lit (Pos 2),Lit (Pos 3))) in 
  print_string ( testCNF (formulaeToCnf [exn]) ) ;;
