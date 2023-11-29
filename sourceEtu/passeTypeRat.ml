(* Module de la passe de gestion des identifiants *)
(* doit être conforme à l'interface Passe *)
open Tds
open Type
open Exceptions
open Ast

type t1 = Ast.AstSyntax.programme
type t2 = Ast.AstTds.programme

(* TO DO*)
let rec analyse_type_expression tds e = 
  match e with 
  | AstSyntax.AppelFonction(nom, el) -> failwith "To Do" (*let l = List.map (analyse_type_expression tds) el in
                                        let nle = List.map fst l in
                                        let nlt = List.map snd l in
                                          if EstCompatible(tp, nlt) then 
                                            AstType.AppelFonction (info, nle)
                                          else raise (TypeInattendu nlt)*)
                                        
  | AstSyntax.Ident (blc) -> (match chercherGlobalement tds blc with
                              | None ->  raise (IdentifiantNonDeclare blc)
                              | Some info -> (match info_ast_to_info info with
                                              | InfoConst (_,_) -> AstType.Ident (info)
                                              | InfoVar (_,_,_,_) -> AstType.Ident (info)
                                              | _ -> raise (MauvaiseUtilisationIdentifiant blc)
                              ))
  | AstSyntax.Booleen (b) -> AstType.Booleen (b)
  | AstSyntax.Entier (i) -> AstType.Entier (i)
  | AstSyntax.Unaire (op, exp) -> AstType.Unaire (op, analyse_type_expression tds exp)
  | AstSyntax.Binaire (op, exp1, exp2) -> let (ne1, t1) = analyse_type_expression tds exp1 in
                                          let (ne2, t2) = analyse_type_expression tds exp2 in
                                          (match t1 op t2 with 
                                          | Int, Plus, Int -> AstType.Binaire (PlusInt ne1 ne2)
                                          | Rat, Plus, Rat -> AstType.Binaire (PlusRat ne1 ne2)
                                          )
  | _ -> failwith "Erreur dans le match"

(* TO DO*)
let rec analyse_type_instruction tds oia i =
  match i with
  | AstSyntax.Declaration (t, n, e) -> (let (ne, te) = analyse_type_expression e in 
                                        if EstCompatible(t, te) then 
                                          let info = (info_to_info_ast (InfoVar (n, t, 0, ""))) in
                                          ajouter tds n info;
                                          AstType.Declaration (t, info, ne)
                                        else raise (TypeInattendu te))
  | AstSyntax.Affectation (n,e) -> let (ne, te) = analyse_type_expression e in
                                   match chercherGlobalement tds n with
                                   | Some info -> (match info_ast_to_info info with
                                                   | InfoVar (n, t, _, _) -> if EstCompatible(t, te) then AstType.Affectation (info, ne)
                                                                              else raise (TypeInattendu te)
                                                   | _ -> failwith "Erreur dans le match")
                                   | None -> raise (VariableNonDeclaree n)
      
  | AstSyntax.Constante (n,v) -> let info = (info_to_info_ast (InfoConst (n, v))) in
                                 ajouter tds n info;
                                 AstType.Constante (info)
      
  | AstSyntax.Affichage e -> (let (ne, te) = analyse_type_expression e in
                              match te with
                              | Type.Int -> AstType.AffichageInt ne
                              | Type.Rat -> AstType.AffichageRat ne
                              | Type.Bool -> AstType.AffichageBool ne
                              | _ -> raise (TypeInattendu te))
      
  | AstSyntax.Conditionnelle (c,t,e) -> (let (nc, tc) = analyse_type_expression c in
                                         match tc with
                                         | Type.Bool -> let nt = analyse_type_bloc tds oia t in
                                                        let ne = analyse_type_bloc tds oia e in
                                                        AstType.Conditionnelle (nc, nt, ne)
                                         | _ -> raise (TypeInattendu tc))
      
  | AstSyntax.TantQue (c,b) -> (let (nc, tc) = analyse_type_expression c in
                                match tc with
                                | Type.Bool -> let nb = analyse_type_bloc tds oia b in
                                               AstType.TantQue (nc, nb)
                                | _ -> raise (TypeInattendu tc))
      
  | AstSyntax.Retour (e) -> (let (ne, te) = analyse_type_expression e in
                            match oia with
                            | Some ia -> (match info_ast_to_info ia with
                                          | InfoFun (n, t, lp) -> if EstCompatible(t, te) then AstType.Retour (ne)
                                                                    else raise (TypeInattendu te)
                                          | _ -> failwith "Erreur dans le match")
                            | None -> raise (RetourEnDehorsFonction))
     


(* analyse_tds_bloc : tds -> info_ast option -> AstSyntax.bloc -> AstTds.bloc *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre oia : None si le bloc li est dans le programme principal,
                   Some ia où ia est l'information associée à la fonction dans laquelle est le bloc li sinon *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le bloc en un bloc de type AstTds.bloc *)
(* Erreur si mauvaise utilisation des identifiants *)
and analyse_tds_bloc tds oia li =
  (* Entrée dans un nouveau bloc, donc création d'une nouvelle tds locale
  pointant sur la table du bloc parent *)
  let tdsbloc = creerTDSFille tds in
  (* Analyse des instructions du bloc avec la tds du nouveau bloc.
     Cette tds est modifiée par effet de bord *)
   let nli = List.map (analyse_tds_instruction tdsbloc oia) li in
   (* afficher_locale tdsbloc ; *) (* décommenter pour afficher la table locale *)
   nli


   let analyse_tds_parametre tds (t,n) =
    match chercherLocalement tds n with 
    | Some a -> raise (DoubleDeclaration n)
    | None -> let pointeur = (info_to_info_ast (InfoVar (n, Undefined, 0, ""))) in
      ajouter tds n pointeur;
      (t, pointeur)

(* TO DO*)
let analyse_tds_fonction maintds (AstSyntax.Fonction(t,n,lp,li))  = failwith "TO DO"


(* analyser : AstSyntax.programme -> AstTds.programme *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le programme
en un programme de type AstTds.programme *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyser (AstSyntax.Programme (fonctions,prog)) =
  let tds = creerTDSMere () in
  let nf = List.map (analyse_tds_fonction tds) fonctions in
  let nb = analyse_tds_bloc tds None prog in
  AstTds.Programme (nf,nb)
