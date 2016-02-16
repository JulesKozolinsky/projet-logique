
(* implémentation de WeakHash où m est vecteur de 512 caractères et renvoie un digest de 128 bits*) 
let xor p q = (p && not(q)) || (not(p) && q) ;;

(*** Main function ***)
let compute input =
        let m = input in
        let d = Array.make 128 false in
        for i = 0 to 10 do
                d.(i) <- xor (xor d.(i+10)           
                                  m.((i*13)   mod 512) )
                             (xor m.((i*14+1) mod 512)
                                  m.((i*15+2) mod 512) )
        done;
        d
;;





let genCNF digest =
        let d = digest in
        let formula_list = ref [] in
        for i = 0 to 10 do                  
                formula_list := (Equiv ( Const d.(i),
                                        Xor (Xor (Const (d.(i+10)),
                                                  Lit(Pos( ((i*13) mod 512)+1 ))) ,
                                             Xor (Lit(Pos( ((i*14+1) mod 512)+1)) ,
                                                  Lit(Pos( ((i*15+2) mod 512)+1))) ))) ::(!formula_list);
        done;
        formula_list := (Or (Lit(Pos (512)),Lit(Neg (512))))::(!formula_list);
        Formula.formulaeToCnf(!formula_list)
;;
    	
		
		
