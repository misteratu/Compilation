(* Module de la passe de gestion des identifiants *)
(* doit être conforme à l'interface Passe *)
open Ast
open Tds
open Type

type t1 = Ast.AstType.programme
type t2 = Ast.AstPlacement.programme



let rec analyser_placement_bloc li depl reg = 
  match li with
  | [] -> ([], 0)
  | t::q -> 
    let (nt, taille_t) = analyser_placement_instruction t depl reg in
    let (nq, taille_q) = analyser_placement_bloc q (taille_t + depl) reg in
    nt::nq, taille_t + taille_q;

and analyser_placement_instruction i depl reg =
  match i with 
  | AstType.Affectation (info, e) -> (AstPlacement.Affectation(info, e),0)
  | AstType.Conditionnelle (c, b1, b2) -> (let nb1 = analyser_placement_bloc b1 depl reg in
                                          let nb2 = analyser_placement_bloc b2 depl reg in
                                          AstPlacement.Conditionnelle(c, nb1, nb2), 0)
  | AstType.Declaration (info, e) -> (match info_ast_to_info info with
                                  | InfoVar(_, t, _, _) -> modifier_adresse_variable depl reg info;
                                                            (AstPlacement.Declaration(info, e), getTaille t)
                                  | _ -> failwith "erreur dans la declaration de variable")
  | AstType.Retour (e, info) -> (match info_ast_to_info info with
                              | InfoFun (_, tr, tp) -> (AstPlacement.Retour(e, getTaille tr, List.fold_right (fun x acc -> getTaille x + acc) tp 0), 0)
                              | _ -> failwith "erreur dans la declaration de fonction")
  | AstType.AffichageInt e -> (AstPlacement.AffichageInt e, 0)
  | AstType.AffichageRat e -> (AstPlacement.AffichageRat e, 0)
  | AstType.AffichageBool e -> (AstPlacement.AffichageBool e, 0)
  | AstType.TantQue (e, b) -> (let nb = analyser_placement_bloc b depl reg in
                              AstPlacement.TantQue(e, nb), 0)
  | AstType.Empty -> (AstPlacement.Empty, 0)

let analyser_placement_fonction (AstType.Fonction(inf_fonc, inf_param, b)) = 
  let rev = List.rev inf_param in
  let rec mod_all_var l pos = 
    match l with
    | [] -> []
    | t::q -> (match info_ast_to_info t with
              | InfoVar(_, ty, _, _) -> modifier_adresse_variable pos "LB" t;
                                        mod_all_var q (pos - (getTaille ty));
              | _ -> failwith "erreur dans la declaration de variable")
  in let _ = mod_all_var rev (-1) in 
  AstPlacement.Fonction(inf_fonc, inf_param, analyser_placement_bloc b 3 "LB")
 

let analyser (AstType.Programme(fonctions, prog)) = 
  let nlf = List.map analyser_placement_fonction fonctions in
  let nb = analyser_placement_bloc prog 0 "SB" in
  AstPlacement.Programme(nlf, nb)