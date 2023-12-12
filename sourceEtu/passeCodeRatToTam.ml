(* Module de la passe de gestion des identifiants *)
(* doit être conforme à l'interface Passe *)
open Ast
open Tds
open Tam
open Type
open Code


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
  | AstType.Unaire(op, e) ->  let res = analyser_code_expression e in
                                (match op with
                                  | Numerateur -> res^pop (0) 1
                                  | Denominateur -> res^pop (1) 1)
  | AstType.Binaire(op, e1, e2) ->  let res1 = analyser_code_expression e1 in
                                    let res2 = analyser_code_expression e2 in
                                    let res = res1^res2 in
                                        (match op with
                                        | AstType.Fraction -> res
                                        | AstType.PlusInt ->  res^(subr "IAdd")
                                        | AstType.PlusRat ->  res^(call "ST" "RAdd")
                                        | AstType.MultInt ->  res^(subr "IMul")
                                        | AstType.MultRat ->  res^(call "ST" "RMul")
                                        | AstType.EquInt | AstType.EquBool ->  res^(subr "IEq")
                                        | AstType.Inf ->      res^(subr "ILss"))


let rec analyser_code_bloc b = 
  match b with 
  |(li, taille) -> (String.concat "" (List.map analyser_code_instruction li)) ^
                    (pop (0) taille)

and analyser_code_instruction i =
  match i with 
  | AstPlacement.Declaration (inf,expr) -> let res = analyser_code_expression expr in
                                            (match info_ast_to_info inf with
                                            | InfoVar(_,t,d,reg) ->  push (getTaille t) ^ res ^ store (getTaille t) d reg
                                            | _ -> failwith "impossible")
  | AstPlacement.Affectation (inf,expr) ->  let res = analyser_code_expression expr in 
                                            (match info_ast_to_info inf with
                                            | InfoVar(_,t,d,reg) -> res ^ store (getTaille t) d reg
                                            | _ -> failwith "impossible")
  | AstPlacement.AffichageInt expr -> analyser_code_expression expr ^ subr "IOut"
  | AstPlacement.AffichageRat expr -> analyser_code_expression expr ^ call "ST" "ROut"
  | AstPlacement.AffichageBool expr -> analyser_code_expression expr ^ subr "BOut"
  | AstPlacement.Conditionnelle (expr,b1,b2) -> let sinon = getEtiquette() in
                                                let fin = getEtiquette() in
                                                analyser_code_expression expr ^
                                                jumpif 0 sinon ^
                                                analyser_code_bloc b1 ^
                                                jump fin ^
                                                label sinon ^
                                                analyser_code_bloc b2 ^
                                                label fin 
                                                
  | AstPlacement.TantQue (expr,b) ->  let debut = getEtiquette() in
                                      let fin = getEtiquette() in
                                      label debut ^
                                      analyser_code_expression expr ^
                                      jumpif 0 fin ^
                                      analyser_code_bloc b ^
                                      jump debut ^
                                      label fin
                                        
  | AstPlacement.Retour (expr,taille_ret,taille_param) -> analyser_code_expression expr ^ return taille_ret taille_param
  | AstPlacement.Empty -> ""



let analyser_placement_fonction (AstType.Fonction(inf_fonc, inf_param, b)) = failwith "caca"
 

let analyser (AstPlacement.Programme(fonctions, prog)) = 
  let code = ("main \n" ^ analyser_code_bloc prog ^ halt ) in
  let analys = getEntete() ^ code in
  (* let nlf = List.map analyser_code_fonction fonctions in *)
  (* print_endline code; *)
  analys