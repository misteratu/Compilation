(* Module de la passe de gestion des identifiants *)
(* doit être conforme à l'interface Passe *)
open Tds
open Type
open Exceptions
open Ast

type t1 = Ast.AstTds.programme
type t2 = Ast.AstType.programme


(*
  [get_type_info : info_ast -> typ]

  Cette fonction renvoie le type associé à une information (constante, variable, fonction).

  Paramètre :
  - Information (constante, variable, fonction).

  Résultat :
  - Type associé à l'information.
*)
let get_type_info info = 
  match info with
  | InfoConst (_,_) -> Int            (* Si c'est une constante, le type est Int *)
  | InfoVar (_,t,_,_) -> t            (* Si c'est une variable, on renvoie son type *)
  | InfoFun (_,t,_) -> t              (* Si c'est une fonction, on renvoie son type *)
  | _ -> failwith "Erreur dans le get_type_info"

(*
  [analyse_type_expression : AstType.expression -> typ]

  Cette fonction analyse le type d'une expression.

  Paramètre :
  - Expression à analyser.

  Résultat :
  - Type de l'expression.

  Erreur :
  - Si le type de l'expression n'est pas conforme aux attentes, des exceptions de type "TypeInattendu" sont levées.
*)
let rec get_typ_expression e = 
  (* Analyse de l'expression *)
  match e with
  | AstType.Affectable (a) -> (match a with
                              | AstType.Ident(ia) ->
                                (* Renvoie d'un couple composé du type de l'identifiant et du nouvel Ident *)
                                get_type_info (info_ast_to_info ia)
                              | AstType.Deref (_, t) -> t
                              | AstType.TabInd (_, _, t) -> t
                                )
  | AstType.Booleen _-> Bool 
  | AstType.Entier _-> Int
  | AstType.Unaire (op,exp) -> (match exp with
                                | AstType.Binaire (_,e1,e2) ->  
                                  (match op with
                                  | Numerateur -> get_typ_expression e1
                                  | Denominateur -> get_typ_expression e2)
                                | AstType.Affectable (a) -> (match a with 
                                                          | AstType.Ident id -> let inf = info_ast_to_info id in
                                                                                (match inf with
                                                                                | InfoConst (_,_) -> Int
                                                                                | InfoVar (_,t,_,_)
                                                                                | InfoFun (_,t,_) -> if t=Rat then
                                                                                                      Int
                                                                                                  else raise  (TypeInattendu (t,Rat))
                                                                                | _ -> failwith "Mauvaise info pour ident"
                                                                                )
                                                          | AstType.Deref (_, t) -> t
                                                          | AstType.TabInd (_, _, t) -> t
                                                          )
                                                          
                                | _ -> raise (TypeInattendu(get_typ_expression exp,Rat))
                                )
  | AstType.Binaire (op,_,_) -> (* Gestions de toutes les opérations possibles *)
                                (match op with
                                  | Fraction -> Rat
                                  | PlusInt -> Int
                                  | PlusRat -> Rat
                                  | MultInt -> Int
                                  | MultRat -> Rat
                                  | EquInt -> Bool
                                  | EquBool -> Bool
                                  | Inf -> Bool)
  | AstType.AppelFonction (infa,_) -> let inf = info_ast_to_info infa in
                                      (match inf with
                                      | InfoFun (_,t,_) -> t
                                      | _ -> failwith "Non InfoFun pour fonction")
  | AstType.New t -> Pointer t
  | AstType.Null -> Pointer (Undefined)
  | AstType.Adresse info -> let t = get_type_info (info_ast_to_info info) in Pointer(t)
  | AstType.NewTab (t,_) -> Tab t
  (* Pour tester les valeurs, on regarde le type de la premiere puis on verifie que c'est les meme types ensuite.
     Le type a la sortie est Tab Undifined si il n'y a pas eu le meme type un moment, Tab type sinon *)
  | AstType.ListeValeurs (el,_) ->  let tl = List.map get_typ_expression el in
                                  let origine = List.hd tl in 
                                  let res = List.fold_left (fun a b -> if a=b then a else Undefined) origine tl in 
                                    if res = Undefined then
                                      Undefined
                                    else Tab(res)


(*
  [get_typ_list : AstType.expression list -> typ list]

  Cette fonction réalise l'analyse de type pour une liste d'expressions.

  Paramètre :
  - Liste d'expressions à analyser.

  Résultat :
  - Liste de types associés aux expressions.
*)
let get_typ_list el = List.map (get_typ_expression) el


(*
  [analyse_type_affectable : AstTds.affectable -> typ * AstType.affectable]

  Cette fonction analyse le type d'un affectable (variable ou déréférencement).

  Paramètre :
  - Affectable à analyser.

  Résultat :
  - Couple composé du type de l'affectable et de l'affectable transformé en AstType.
  
  Erreur :
  - Si le type de l'affectable n'est pas conforme aux attentes, une exception de type "TypeInattendu" est levée.
*)
let rec analyse_type_affectable a =
  match a with
  | AstTds.Ident(ia) ->
    (* Renvoie d'un couple composé du type de l'identifiant et du nouvel Ident *)
    (get_type_info (info_ast_to_info ia), AstType.Ident(ia))
  | AstTds.Deref da ->
    (* Analyse de l'affectable *)
    (* *x -> Pointeur Int -> Int *)
    let (ta, nda) = analyse_type_affectable da in
    (match ta with
      | Pointer(typ) -> (typ, AstType.Deref(nda,typ))
      | _ -> raise (TypeInattendu(ta,Pointer(Undefined))))
      (* int [] a = ...*)
  | AstTds.TabInd (a, e) -> let (ta, na) = analyse_type_affectable a in
                            let ne = analyse_type_expression e in 
                            let te = get_typ_expression ne in
                            if te!=Int then raise  (TypeInattendu(te,Int))
                            else (match ta with
                                  | Tab(tt) -> (tt, AstType.TabInd(na, ne, tt))
                                  | _ -> raise (TypeInattendu(ta,Tab(Undefined))))
                            

(*
  [analyse_type_expression : AstTds.expression -> AstType.expression]

  Cette fonction réalise l'analyse de type pour une expression AstTds.

  Paramètre :
  - Expression AstTds à analyser.

  Résultat :
  - Expression AstType correspondant à l'analyse de type de l'expression.
*)
and analyse_type_expression e = 
  match e with 
  | AstTds.AppelFonction(info, el) -> (* Récuération de la liste de expressions *)
                                      let el_n = List.map (analyse_type_expression) el in
                                      (* Récupération de la liste de types associés aux expressions *)
                                      let tl = get_typ_list el_n in
                                      (* Récupération de l'information de la fonction *)
                                      let inf = info_ast_to_info info in
                                      (match inf with
                                      | InfoFun (_,_, tlf) -> (* Vérification de la comptabilité des différents types *)
                                                              if est_compatible_list tl tlf then AstType.AppelFonction (info, el_n)
                                                              else raise (TypesParametresInattendus (tl,tlf))
                                      | _ -> failwith "Non InfoFun pour fonction"
                                      )
              
  | AstTds.Affectable (a) ->  (* Analyse de l'affectable *)
                              let (_, na) = analyse_type_affectable a in 
                              (* Création d'un affectable dans AstType*)
                              AstType.Affectable(na)
                            
  | AstTds.Booleen (b) -> AstType.Booleen (b)
  | AstTds.Entier (i) -> AstType.Entier (i)
  | AstTds.Unaire (op, exp) -> (* Gestion des opérations de unaire *)
                              (match op with 
                                | AstSyntax.Denominateur -> AstType.Unaire (Denominateur, analyse_type_expression exp)
                                | AstSyntax.Numerateur -> AstType.Unaire (Numerateur, analyse_type_expression exp)
                              )
  | AstTds.Binaire (op, exp1, exp2) ->  (* Analyse de la première expression *)
                                        let ne1 = analyse_type_expression exp1 in
                                        (* Analyse de la seconde expression *)
                                        let ne2 = analyse_type_expression exp2 in
                                        (* Récupération des types associés aux expressions *)
                                        let t1 = get_typ_expression ne1 in
                                        let t2 = get_typ_expression ne2 in
                                        (* Création d'une expression binaire dans AstType en fonction de l'opération*)
                                          (match t1, op, t2 with 
                                          | Int, Plus, Int -> AstType.Binaire (PlusInt, ne1, ne2)
                                          | Rat, Plus, Rat -> AstType.Binaire (PlusRat, ne1, ne2)
                                          | Int, Mult, Int -> AstType.Binaire (MultInt, ne1, ne2)
                                          | Rat, Mult, Rat -> AstType.Binaire (MultRat, ne1, ne2)
                                          | Int, Fraction, Int -> AstType.Binaire (Fraction, ne1, ne2)
                                          | Int, Equ, Int -> AstType.Binaire (EquInt, ne1, ne2)
                                          | Bool, Equ, Bool -> AstType.Binaire (EquBool, ne1, ne2)
                                          | Int, Inf, Int -> AstType.Binaire (Inf, ne1, ne2)
                                          | _ -> raise (TypeBinaireInattendu (op, t1, t2)))
  | AstTds.Null -> AstType.Null
  | AstTds.New t -> AstType.New t
  | AstTds.Adresse (info) -> AstType.Adresse (info)
  | AstTds.NewTab (t, e) -> AstType.NewTab (t, analyse_type_expression e)
  | AstTds.ListeValeurs (el) -> let res = List.map (analyse_type_expression) el in
                                let t = get_typ_expression (List.hd res) in
                                  AstType.ListeValeurs (res,t)
  


(*
  [analyse_type_instruction : AstTds.instruction -> AstType.instruction]

  Cette fonction effectue l'analyse de type pour une instruction de type AstTds et renvoie
  une instruction de type AstType. Elle assure la cohérence des types lors de l'analyse.

  Paramètre :
  - Instruction AstTds à analyser.

  Résultat :
  - Instruction AstType résultant de l'analyse de type.

  Erreurs possibles :
  - [TypeInattendu] : En cas d'incompatibilité des types lors de l'analyse.
*)

let rec analyse_type_instruction i =
  match i with
  | AstTds.Declaration (t, n, e) ->
      (* Analyse de l'expression *)
      let ne = analyse_type_expression e in 
      (* Récupération du type associé à l'expression *)
      let te = get_typ_expression ne in
      (* Vérification de la compatibilité des types *)
      if est_compatible t te then
        (* Ajout de la variable à la tds *)
        (modifier_type_variable t n;
         AstType.Declaration (n, ne))
      else (
        raise (TypeInattendu (te,t)))

  | AstTds.Affectation (info,e) ->
      (* Analyse de l'expression *)
      let ne = analyse_type_expression e in 
      (* Récupération du type associé à l'expression *)
      let te = get_typ_expression ne in
      (* Récupération de l'information associée à l'affectable *)
      let (t, a) = analyse_type_affectable info in
      if est_compatible t te then AstType.Affectation (a, ne) 
      else raise (TypeInattendu (te, t));

  | AstTds.Affichage e ->
      (* Analyse de l'expression *)
       (let ne = analyse_type_expression e in 
       (* Récupération du type associé à l'expression *)
       let te = get_typ_expression ne in
        (* Création d'un affichage dans AstType en fonction du type de l'expression *)
       match te with
       | Int -> AstType.AffichageInt ne
       | Rat -> AstType.AffichageRat ne
       | Bool -> AstType.AffichageBool ne
       | _ -> failwith "Erreur dans l'affichage")

  | AstTds.Conditionnelle (c,t,e) ->
      (* Analyse de l'expression *)
      (let nc = analyse_type_expression c in 
      (* Récupération du type associé à l'expression *)
       let tc = get_typ_expression nc in
      (* Analyse du bloc then *)
       match tc with
       | Type.Bool ->
          let nt = analyse_type_bloc t in
          let ne = analyse_type_bloc e in
          AstType.Conditionnelle (nc, nt, ne)
       | _ -> raise (TypeInattendu(tc,Bool)))

  | AstTds.TantQue (c,b) ->
      (let nc = analyse_type_expression c in 
       let tc = get_typ_expression nc in
       match tc with
       | Bool ->
           let nb = analyse_type_bloc b in
           AstType.TantQue (nc, nb)
       | _ -> raise (TypeInattendu(tc,Bool)))

  | AstTds.Retour (e, info) ->
      (let ne = analyse_type_expression e in 
       let te = get_typ_expression ne in
       match info_ast_to_info info with
       | InfoFun (_, t, _) -> if est_compatible t te then AstType.Retour (ne, info)
                             else raise (TypeInattendu (te,t))
       | _ -> failwith "Erreur dans le retour")

  | AstTds.Empty -> AstType.Empty
  | AstTds.For (i1, e2, a, e3, b) ->
      let ne1 = analyse_type_instruction i1 in
      let te1 = (match ne1 with
                | AstType.Declaration (info, _) -> (match info_ast_to_info info with
                                                    | InfoVar (_, t, _, _) -> if t=Int then Int
                                                                              else raise (TypeInattendu (t, Int))
                                                    | _ -> failwith "Erreur dans le for")
                | _ -> failwith "Impossible for"  ) in
       let ne2 = analyse_type_expression e2 in
       let ne3 = analyse_type_expression e3 in
       let te2 = get_typ_expression ne2 in
       let ta, na = analyse_type_affectable a in
       let te3 = get_typ_expression ne3 in
       (match te1, te2, ta, te3 with
       | Int, Bool, Int, Int ->
           let nb = analyse_type_bloc b in
           AstType.For (ne1, ne2, na, ne3, nb)
       | _ -> raise (TypeInattendu (te1, Int)))
  | AstTds.Goto (is) -> AstType.Goto (is)
  | AstTds.Label (is) -> AstType.Label (is)



(* analyse_tds_bloc : tds -> info_ast option -> AstTds.bloc -> AstTds.bloc *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le bloc en un bloc de type AstTds.bloc *)
(* Erreur si mauvaise utilisation des identifiants *)
and analyse_type_bloc li =

  (* Analyse des instructions du bloc avec la tds du nouveau bloc.
     Cette tds est modifiée par effet de bord *)
   let nli = List.map (analyse_type_instruction) li in
   (* afficher_locale tdsbloc ; *) (* décommenter pour afficher la table locale *)
   nli


(*
  [analyse_type_fonction : AstTds.fonction -> AstType.fonction]

  Cette fonction effectue l'analyse de type pour une fonction de type AstTds et renvoie
  une fonction de type AstType. Elle assure la cohérence des types lors de l'analyse.

  Paramètres :
  - Fonction AstTds à analyser.

  Résultat :
  - Fonction AstType résultant de l'analyse de type.

  Erreurs possibles :
  - [TypeInattendu] : En cas d'incompatibilité des types lors de l'analyse.
*)

let analyse_type_fonction (AstTds.Fonction(t,inf_fonc,l_param,li)) =
  (* Ajout du type des paramètres à la tds *)
  let l_typ_param = List.map (fun (t,_) -> t) l_param in
  modifier_type_fonction t l_typ_param inf_fonc;
  let _ = List.map (fun (t,i) -> modifier_type_variable t i) l_param in
  let l_param_new = List.map (fun (_,i) -> i) l_param in
  (* Analyse du bloc *)
  let nli = analyse_type_bloc li in
  (* Création d'une fonction dans AstType *)
  AstType.Fonction(inf_fonc,l_param_new,nli)


(* analyser : AstTds.programme -> AstTds.programme *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le programme
en un programme de type AstTds.programme *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyser (AstTds.Programme (fonctions,prog)) =
  let nf = List.map analyse_type_fonction fonctions in
  let nb = analyse_type_bloc prog in
  AstType.Programme (nf,nb)