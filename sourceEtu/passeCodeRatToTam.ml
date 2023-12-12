(* Module de la passe de gestion des identifiants *)
(* doit être conforme à l'interface Passe *)
open Ast
open Tds
open Tam
open Type


type t1 = Ast.AstPlacement.programme
type t2 = string


let rec analyser_code_expression e = 
  match e with
  | AstType.AppelFonction (info, l) -> failwith "caca"
  | AstType.Entier(i) -> loadl_int i
  | AstType.Booleen(b) -> (match b with
                              | true -> loadl_int 1
                              | false -> loadl_int 0 )
  | AstType.Ident(info) -> (match info_ast_to_info info with 
                                | InfoVar(_, t, depl, reg) -> load (getTaille t) depl reg
                                | InfoConst(_, i) -> loadl_int (i)
                                | _ -> failwith "erreur dans la declaration de variable")
  | AstType.Unaire(op, _) -> (match op with
                                  | AstType.Numerateur -> pop (0) 1
                                  | AstType.Denominateur -> pop (1) 1)
  | AstType.Binaire(op, e1, e2) ->  let _ = analyser_code_expression e1 in
                                    let _ = analyser_code_expression e2 in
                                        (match op with
                                        | AstType.PlusInt ->  subr "IAdd"
                                        | AstType.PlusRat ->  call "SB" "RAdd"
                                        | AstType.MultInt ->  subr "IMul"
                                        | AstType.MultRat ->  call "SB" "RMul"
                                        | AstType.Fraction -> ""
                                        | AstType.EquInt ->   subr "IEq"
                                        | AstType.Inf ->      subr "ILess";
                                        | AstType.EquBool ->  subr "IEq")


let rec analyser_code_bloc b = 
  match b with 
  |(li, taille) -> let _ = String.concat "" (List.map analyser_code_instruction li) in
                   pop (0) taille

and analyser_code_instruction i =
  match i with 
  | AstPlacement.Declaration (inf,expr) -> let _ = analyser_code_expression expr in
                                            (match info_ast_to_info inf with
                                            | InfoVar(_,t,d,reg) ->  let _ = push (getTaille t) in
                                                                    store (getTaille t) d reg
                                            | _ -> failwith "impossible")
  | AstPlacement.Affectation (inf,expr) ->  let _ = analyser_code_expression expr in 
                                            (match info_ast_to_info inf with
                                            | InfoVar(_,t,d,reg) -> store (getTaille t) d reg
                                            | _ -> failwith "impossible")
  | AstPlacement.AffichageInt expr -> let res1 = analyser_code_expression expr in
                                      subr "IOut"
  | AstPlacement.AffichageRat expr -> let _ = analyser_code_expression expr in
                                      call "SB" "ROut"
  | AstPlacement.AffichageBool expr -> let _ = analyser_code_expression expr in
                                      subr "BOut"
  | AstPlacement.Conditionnelle (expr,b1,b2) -> let _ = analyser_code_expression expr in
                                                let debut = "SI" in
                                                let fin = "FSI" in
                                                let _ = label debut in
                                                let _ = analyser_code_expression expr in
                                                let _ = jumpif 0 debut in
                                                let _ = analyser_code_bloc b1 in
                                                let _ = jump fin in
                                                let _ = analyser_code_bloc b2 in
                                                label fin
                                                
  | AstPlacement.TantQue (expr,b) ->  let debut = "TQ" in
                                      let fin = "FTQ" in
                                      let _ = label debut in
                                      let _ = analyser_code_expression expr in
                                      let _ = jumpif 0 fin in
                                      let _ = analyser_code_bloc b in
                                      let _ = jump fin in
                                      label fin
                                        
  | AstPlacement.Retour (expr,taille_ret,taille_param) -> let _ = analyser_code_expression expr in
                                                                  return taille_ret taille_param
  | AstPlacement.Empty -> ""



let analyser_placement_fonction (AstType.Fonction(inf_fonc, inf_param, b)) = failwith "caca"
 

let analyser (AstPlacement.Programme(fonctions, prog)) = 
  (* let nlf = List.map analyser_code_fonction fonctions in *)
  analyser_code_bloc prog 