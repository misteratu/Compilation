(* Module de la passe de gestion des identifiants *)
(* doit être conforme à l'interface Passe *)
open Tds
open Exceptions
open Ast
open Code

type t1 = Ast.AstSyntax.programme
type t2 = Ast.AstTds.programme

(* liste globale des etiques seulement utilisées dans un GOTO *)
let etiquettes_non_declarees = ref []  

let supprimer_element x liste =
  List.filter (fun e -> e <> x) liste

(* Fonction vérifiant la présence d'un ou plusieurs GOTO sans label -> si présence alors on déclare une exception *)
let verifier_etiquettes etiquettes =
  if (List.length etiquettes) > 0 then
    raise (IdentifiantNonDeclare (List.hd etiquettes))
  else
    ()

(* Fonction permettant de récupérer directement l'info associé à un affectable *)
let rec affectable_info a = 
  match a with 
  | AstTds.Ident ia -> ia
  | AstTds.Deref ia -> affectable_info ia
  | AstTds.TabInd (ia, _) -> affectable_info ia

(* Renvoie une info ast correspondante a l'affectable, erreur sinon *)
(* modif prend "l" pour lecture et "e" pour ecriture*)
let rec analyse_tds_affectable tds a modif =
  match a with
  | AstSyntax.TabInd (a, e) -> AstTds.TabInd (analyse_tds_affectable tds a modif, analyse_tds_expression tds e)
  | AstSyntax.Ident n -> (match chercherGlobalement tds n with
                          | Some ia -> (match info_ast_to_info ia with
                                        | InfoVar(_,_,_,_) -> AstTds.Ident(ia)
                                        | InfoConst(_,_) -> (* On check ici qu'on ne cherche pas à modifier la valeur d'une constante *)
                                          if (modif == "l") then
                                            Ident(ia)
                                          else 
                                            raise (MauvaiseUtilisationIdentifiant n)
                                        | _ -> raise (MauvaiseUtilisationIdentifiant n))
                          | None -> raise (IdentifiantNonDeclare n))
  | AstSyntax.Deref a -> AstTds.Deref (analyse_tds_affectable tds a modif)

(* analyse_tds_expression : tds -> AstSyntax.expression -> AstTds.expression *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'expression
en une expression de type AstTds.expression *)
(* Erreur si mauvaise utilisation des identifiants *)
and analyse_tds_expression tds e = 
  match e with 
  | AstSyntax.AppelFonction(nom, el) -> (match chercherGlobalement tds nom with 
                                          | None -> raise (IdentifiantNonDeclare nom)
                                          | Some info -> 
                                            (match info_ast_to_info info with
                                            | InfoFun (_,_,_) ->AstTds.AppelFonction(info, List.map (analyse_tds_expression tds) el)
                                            | _ -> raise (MauvaiseUtilisationIdentifiant nom))
                                        )
  | AstSyntax.Affectable a -> let res = analyse_tds_affectable tds a "l" in AstTds.Affectable res
  | AstSyntax.Booleen (b) -> AstTds.Booleen (b)
  | AstSyntax.Entier (i) -> AstTds.Entier (i)
  | AstSyntax.Unaire (op, exp) -> AstTds.Unaire(op, analyse_tds_expression tds exp)
  | AstSyntax.Binaire (op, exp1, exp2) -> AstTds.Binaire(op, analyse_tds_expression tds exp1, analyse_tds_expression tds exp2)
  | AstSyntax.Null -> AstTds.Null
  | AstSyntax.New t -> AstTds.New t
  | AstSyntax.Adresse n -> (match chercherGlobalement tds n with
                            | None -> raise (IdentifiantNonDeclare n)
                            | Some ia -> (match info_ast_to_info ia with
                                          | InfoVar(_,_,_,_) -> AstTds.Adresse ia
                                          | _ -> raise (MauvaiseUtilisationIdentifiant n)))
  | AstSyntax.ListeValeurs lv -> AstTds.ListeValeurs (List.map (analyse_tds_expression tds) lv)
  | AstSyntax.NewTab (t, e) -> AstTds.NewTab (t, analyse_tds_expression tds e)

(* analyse_tds_instruction : tds -> info_ast option -> AstSyntax.instruction -> AstTds.instruction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre oia : None si l'instruction i est dans le bloc principal,
                   Some ia où ia est l'information associée à la fonction dans laquelle est l'instruction i sinon *)
(* Paramètre i : l'instruction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'instruction
en une instruction de type AstTds.instruction *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_instruction tds tdsGOTO maintds oia i =
  match i with
  | AstSyntax.Declaration (t, n, e) ->
      begin
        match chercherLocalement tds n with
        | None ->
            (* L'identifiant n'est pas trouvé dans la tds locale,
            il n'a donc pas été déclaré dans le bloc courant *)
            (* Vérification de la bonne utilisation des identifiants dans l'expression *)
            (* et obtention de l'expression transformée *)
            let ne = analyse_tds_expression tds e in
            (* Création de l'information associée à l'identfiant *)
            let info = InfoVar (n,Undefined, 0, "") in
            (* Création du pointeur sur l'information *)
            let ia = info_to_info_ast info in
            (* Ajout de l'information (pointeur) dans la tds *)
            ajouter tds n ia;
            (* Renvoie de la nouvelle déclaration où le nom a été remplacé par l'information
            et l'expression remplacée par l'expression issue de l'analyse *)
            AstTds.Declaration (t, ia, ne)
        | Some _ ->
            (* L'identifiant est trouvé dans la tds locale,
            il a donc déjà été déclaré dans le bloc courant *)
            raise (DoubleDeclaration n)
      end
  | AstSyntax.Affectation (a,e) ->
    let na = analyse_tds_affectable tds a "e" in 
    let ne = analyse_tds_expression tds e in
    Affectation(na, ne)
      
  | AstSyntax.Constante (n,v) ->
      begin
        match chercherLocalement tds n with
        | None ->
          (* L'identifiant n'est pas trouvé dans la tds locale,
             il n'a donc pas été déclaré dans le bloc courant *)
          (* Ajout dans la tds de la constante *)
          ajouter tds n (info_to_info_ast (InfoConst (n,v)));
          (* Suppression du noeud de déclaration des constantes devenu inutile *)
          AstTds.Empty
        | Some _ ->
          (* L'identifiant est trouvé dans la tds locale,
          il a donc déjà été déclaré dans le bloc courant *)
          raise (DoubleDeclaration n)
      end
  | AstSyntax.Affichage e ->
      (* Vérification de la bonne utilisation des identifiants dans l'expression *)
      (* et obtention de l'expression transformée *)
      let ne = analyse_tds_expression tds e in
      (* Renvoie du nouvel affichage où l'expression remplacée par l'expression issue de l'analyse *)
      AstTds.Affichage (ne)
  | AstSyntax.Conditionnelle (c,t,e) ->
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tds c in
      (* Analyse du bloc then *)
      let tast = analyse_tds_bloc tds tdsGOTO oia t in
      (* Analyse du bloc else *)
      let east = analyse_tds_bloc tds tdsGOTO oia e in
      (* Renvoie la nouvelle structure de la conditionnelle *)
      AstTds.Conditionnelle (nc, tast, east)
  | AstSyntax.TantQue (c,b) ->
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tds c in
      (* Analyse du bloc *)
      let bast = analyse_tds_bloc tds tdsGOTO oia b in
      (* Renvoie la nouvelle structure de la boucle *)
      AstTds.TantQue (nc, bast)
  | AstSyntax.Retour (e) ->
      begin
      (* On récupère l'information associée à la fonction à laquelle le return est associée *)
      match oia with
        (* Il n'y a pas d'information -> l'instruction est dans le bloc principal : erreur *)
      | None -> raise RetourDansMain
        (* Il y a une information -> l'instruction est dans une fonction *)
      | Some ia ->
        (* Analyse de l'expression *)
        let ne = analyse_tds_expression tds e in
        AstTds.Retour (ne,ia)
      end
  | AstSyntax.For (i1, e2, a, e3, b) -> let tdsfor = creerTDSFille tds in
    
                                        let ni1 = (match i1 with
                                        | AstSyntax.Declaration _ -> analyse_tds_instruction tdsfor tdsGOTO maintds oia i1
                                        | _ -> failwith "Mauvaise declaration") in
    
    
                                        let ne2 = (match e2 with
                                        | AstSyntax.Booleen _ -> analyse_tds_expression tdsfor e2
                                        | AstSyntax.Binaire (op, _, _) -> if op == AstSyntax.Equ || op == AstSyntax.Inf then
                                          analyse_tds_expression tdsfor e2 else raise (MauvaiseUtilisationIdentifiant "Opération condition for")
                                        | AstSyntax.Affectable _ -> analyse_tds_expression tdsfor e2
                                        | _ -> failwith "Mauvaise condition d'arret") in

                                        AstTds.For (ni1, ne2, analyse_tds_affectable tdsfor a "e", analyse_tds_expression tdsfor e3, analyse_tds_bloc tdsfor tdsGOTO None b)
  | AstSyntax.Goto (n) ->  (match chercherGlobalement tdsGOTO n with
                            | Some ia ->
                              (match info_ast_to_info ia with
                              | InfoEtiq (_, false, nomGOTO) -> AstTds.Goto (info_to_info_ast (InfoEtiq(n, true, nomGOTO)))
                              | _ -> raise (MauvaiseUtilisationIdentifiant n))
                            | None -> let info = info_to_info_ast (InfoEtiq(n, false, getEtiquette())) in
                                      etiquettes_non_declarees := n :: !etiquettes_non_declarees;
                                      ajouter tdsGOTO n info;
                                      AstTds.Goto (info))
                             
  | AstSyntax.Label (n) -> (match chercherGlobalement tdsGOTO n with
                            | Some ia -> if (List.mem n !etiquettes_non_declarees) then
                              etiquettes_non_declarees := supprimer_element n !etiquettes_non_declarees;
                              (match info_ast_to_info ia with
                              | InfoEtiq (_, false, nomGOTO) -> AstTds.Label (info_to_info_ast (InfoEtiq(n, false, nomGOTO)))
                              | InfoEtiq (_, true, _) -> raise (DoubleDeclaration n)
                              | _ -> raise (MauvaiseUtilisationIdentifiant n))
                            | None -> let info = info_to_info_ast (InfoEtiq(n, true, getEtiquette())) in
                                      ajouter tdsGOTO n info;
                                      AstTds.Label (info))

(* analyse_tds_bloc : tds -> info_ast option -> AstSyntax.bloc -> AstTds.bloc *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre oia : None si le bloc li est dans le programme principal,
                   Some ia où ia est l'information associée à la fonction dans laquelle est le bloc li sinon *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le bloc en un bloc de type AstTds.bloc *)
(* Erreur si mauvaise utilisation des identifiants *)
and analyse_tds_bloc tds tdsGOTO oia li =
  (* Entrée dans un nouveau bloc, donc création d'une nouvelle tds locale
  pointant sur la table du bloc parent *)
  let tdsbloc = creerTDSFille tds in
  (* Analyse des instructions du bloc avec la tds du nouveau bloc.
     Cette tds est modifiée par effet de bord *)
   let nli = List.map (analyse_tds_instruction tdsbloc tdsGOTO tds oia) li in
   (* afficher_locale tdsbloc ; *) (* décommenter pour afficher la table locale *)
   nli

   let analyse_tds_parametre tds (t,n) =
    match chercherLocalement tds n with 
    | Some _ -> raise (DoubleDeclaration n)
    | None -> let pointeur = (info_to_info_ast (InfoVar (n, Undefined, 0, ""))) in
      ajouter tds n pointeur;
      (t, pointeur)

(* analyse_tds_fonction : tds -> AstSyntax.fonction -> AstTds.fonction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre : la fonction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme la fonction
en une fonction de type AstTds.fonction *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyse_tds_fonction maintds tdsGOTO (AstSyntax.Fonction(t,n,lp,li))  = 
  let filletds = creerTDSFille maintds in
  match chercherGlobalement maintds n with
  | None -> let infof_ast = (info_to_info_ast(InfoFun(n, t, (List.map(fun (a,_) -> a) lp)))) in
                ajouter maintds n infof_ast;
                let lparam = List.map (analyse_tds_parametre filletds) lp in 
                let convertBloc = List.map (analyse_tds_instruction filletds tdsGOTO maintds (Some infof_ast)) li in 
            AstTds.Fonction(t, infof_ast, lparam,convertBloc)
  | Some _ -> raise (DoubleDeclaration n)


(* analyser : AstSyntax.programme -> AstTds.programme *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le programme
en un programme de type AstTds.programme *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyser (AstSyntax.Programme (fonctions,prog)) =
  etiquettes_non_declarees := [];
  let tds = creerTDSMere () in
  let tdsGOTO = creerTDSMere () in
  let nf = List.map (analyse_tds_fonction tds tdsGOTO) fonctions in
  let nb = analyse_tds_bloc tds tdsGOTO None prog in
  verifier_etiquettes !etiquettes_non_declarees;
  AstTds.Programme (nf,nb)
