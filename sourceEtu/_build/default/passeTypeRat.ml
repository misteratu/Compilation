(* Module de la passe de gestion des identifiants *)
(* doit être conforme à l'interface Passe *)
open Tds
open Type
open Exceptions
open Ast

type t1 = Ast.AstTds.programme
type t2 = Ast.AstTds.programme

(* analyse_type_expression : AstType.expression -> typ*)
let rec get_typ_expression e = 
  match e with
  | AstType.Ident id -> let inf = info_ast_to_info id in
                    (match inf with
                    | InfoConst (_,_) -> Int
                    | InfoVar (_,t,_,_) -> t
                    | _ -> failwith "Ident pour fonction")
  | AstType.Booleen b-> Bool
  | AstType.Entier ent-> Int
  | AstType.Unaire (_,exp) ->  get_typ_expression exp
  | AstType.Binaire (op,exp1,exp2) -> let t1=get_typ_expression exp1 in
                                  let t2=get_typ_expression exp2 in
                                  if est_compatible t1 t2 then
                                    t1
                                  else
                                    raise (TypeInattendu (t2,t1))
  | AstType.AppelFonction (infa,_) -> let inf = info_ast_to_info infa in
                                      (match inf with
                                      | InfoFun (_,t,_) -> t
                                      | _ -> failwith "Non InfoFun pour fonction")

let get_typ_list el = List.map (get_typ_expression) el


let get_type_info info = 
  match info with
  | InfoConst (_,_) -> Int
  | InfoVar (_,t,_,_) -> t
  | InfoFun (_,t,_) -> t

(* TO DO*)
let rec analyse_type_expression e = 
  match e with 
  | AstTds.AppelFonction(info, el) -> let el_n = List.map (analyse_type_expression) el in
                                      let tl = get_typ_list el_n in
                                      let inf = info_ast_to_info info in
                                      (match inf with
                                      | InfoFun (_,_, tlf) -> if est_compatible_list tl tlf then AstType.AppelFonction (info, el_n)
                                                              else raise (TypesParametresInattendus (tl,tlf))
                                      | _ -> failwith "Non InfoFun pour fonction"
                                      )

                                        
  | AstTds.Ident (blc) -> AstType.Ident (blc)
  | AstTds.Booleen (b) -> AstType.Booleen (b)
  | AstTds.Entier (i) -> AstType.Entier (i)
  | AstTds.Unaire (op, exp) -> (match op with 
                                  | AstSyntax.Denominateur -> AstType.Unaire (Denominateur, analyse_type_expression exp)
                                  | AstSyntax.Numerateur -> AstType.Unaire (Numerateur, analyse_type_expression exp)
                                  )
  | AstTds.Binaire (op, exp1, exp2) ->  let ne1 = analyse_type_expression exp1 in
                                        let ne2 = analyse_type_expression exp2 in
                                        let t1 = get_typ_expression ne1 in
                                        let t2 = get_typ_expression ne2 in
                                          (match t1, op, t2 with 
                                          | Int, Plus, Int -> AstType.Binaire (PlusInt, ne1, ne2)
                                          | Rat, Plus, Rat -> AstType.Binaire (PlusRat, ne1, ne2)
                                          | Int, Mult, Int -> AstType.Binaire (MultInt, ne1, ne2)
                                          | Rat, Mult, Rat -> AstType.Binaire (MultRat, ne1, ne2)
                                          | _, Fraction, _ -> AstType.Binaire (Fraction, ne1, ne2)
                                          | Int, Equ, Int -> AstType.Binaire (EquInt, ne1, ne2)
                                          | Bool, Equ, Bool -> AstType.Binaire (EquBool, ne1, ne2)
                                          | _, Inf, _ -> AstType.Binaire (Inf, ne1, ne2)
                                          | _ -> raise (TypeBinaireInattendu (op, t1, t2)))
  | _ -> failwith "Erreur dans le match"

(* TO DO*)
let rec analyse_type_instruction i =
  match i with
  | AstTds.Declaration (t, n, e) -> let ne = analyse_type_expression e in 
                                      let te = get_typ_expression ne in
                                        if est_compatible t te then
                                          (modifier_type_variable te n;
                                          AstType.Declaration (n, ne))
                                        else 
                                          raise (TypeInattendu (te,t))
  | AstTds.Affectation (info,e) -> let ne = analyse_type_expression e in 
                                  let te = get_typ_expression ne in
                                  (match info_ast_to_info info with
                                  | InfoConst (_, _) ->  if est_compatible Int te then AstType.Affectation (info, ne)
                                  else raise (TypeInattendu (te,Int))
                                  | InfoVar (_, t, _, _) -> if est_compatible t te then AstType.Affectation (info, ne)
                                                    else raise (TypeInattendu (te,t))
                                  | InfoFun (_, t, _) -> if est_compatible t te then AstType.Affectation (info, ne)
                                                       else raise (TypeInattendu (te,t))
                                  )
  | AstTds.Affichage e -> (let ne = analyse_type_expression e in 
                            let te = get_typ_expression ne in
                              match te with
                              | Int -> AstType.AffichageInt ne
                              | Rat -> AstType.AffichageRat ne
                              | Bool -> AstType.AffichageBool ne
                              | _ -> failwith "Erreur dans l'affichage")
      
  | AstTds.Conditionnelle (c,t,e) -> (let nc = analyse_type_expression c in 
                                        let tc = get_typ_expression nc in
                                         match tc with
                                         | Type.Bool -> let nt = analyse_type_bloc t in
                                                        let ne = analyse_type_bloc e in
                                                        AstType.Conditionnelle (nc, nt, ne)
                                         | _ -> failwith "Erreur dans la conditionnelle")
      
  | AstTds.TantQue (c,b) -> (let nc = analyse_type_expression c in 
                              let tc = get_typ_expression nc in
                                match tc with
                                | Bool -> let nb = analyse_type_bloc b in
                                               AstType.TantQue (nc, nb)
                                | _ -> failwith "Erreur dans le tantque")
      
  | AstTds.Retour (e, info) -> (let ne = analyse_type_expression e in 
                                let te = get_typ_expression ne in
                                  match info_ast_to_info info with
                                  | InfoFun (_, t, _) -> if est_compatible t te then AstType.Retour (ne, info)
                                                        else raise (TypeInattendu (te,t))
                                  | _ -> failwith "Erreur dans le retour")
  | AstTds.Empty -> AstType.Empty


(* analyse_tds_bloc : tds -> info_ast option -> AstTds.bloc -> AstTds.bloc *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre oia : None si le bloc li est dans le programme principal,
                   Some ia où ia est l'information associée à la fonction dans laquelle est le bloc li sinon *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le bloc en un bloc de type AstTds.bloc *)
(* Erreur si mauvaise utilisation des identifiants *)
and analyse_type_bloc li =

  (* Analyse des instructions du bloc avec la tds du nouveau bloc.
     Cette tds est modifiée par effet de bord *)
   let nli = List.map (analyse_type_instruction) li in
   (* afficher_locale tdsbloc ; *) (* décommenter pour afficher la table locale *)
   nli


(* TO DO*)
let analyse_type_fonction (AstTds.Fonction(t,n,lp,li))  = failwith "TO DO"


(* analyser : AstTds.programme -> AstTds.programme *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le programme
en un programme de type AstTds.programme *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyser (AstTds.Programme (fonctions,prog)) =
  let tds = creerTDSMere () in
  let nf = List.map (analyse_tds_fonction tds) fonctions in
  let nb = analyse_tds_bloc tds None prog in
  AstTds.Programme (nf,nb)
