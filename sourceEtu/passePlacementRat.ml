(* Module de la passe de gestion des identifiants *)
(* doit être conforme à l'interface Passe *)
open Ast
open Tds
open Type

type t1 = Ast.AstType.programme
type t2 = Ast.AstPlacement.programme



(*
  [analyser_placement_bloc : AstType.instruction list -> int -> string -> (AstPlacement.instruction list * int)]
  
  Cette fonction récursive analyse le placement des instructions dans un bloc.

  Paramètres :
  - li : Liste d'instructions à analyser dans le bloc.
  - depl : Décalage initial pour le placement des variables locales.
  - reg : Registre de base pour les adresses mémoire des variables.

  Résultat :
  - Tuple contenant :
    - Liste d'instructions avec leur placement modifié.
    - Taille totale du bloc, qui représente la somme des tailles de toutes les instructions du bloc.
*)
let rec analyser_placement_bloc li depl reg = 
  match li with
  | [] -> ([], 0) (* Cas de base : si la liste est vide, retourne une liste vide d'instructions et une taille nulle. *)
  | t::q -> 
    (* Analyse le placement de l'instruction courante dans le bloc *)
    let (nt, taille_t) = analyser_placement_instruction t depl reg in
    (* Analyse le placement du reste du bloc récursivement *)
    let (nq, taille_q) = analyser_placement_bloc q (taille_t + depl) reg in
    (* Combine les résultats pour obtenir le placement du bloc entier *)
    nt::nq, taille_t + taille_q;


(*
  [analyser_placement_instruction : AstType.instruction -> int -> string -> (AstPlacement.instruction * int)]

  Cette fonction analyse le placement d'une instruction dans le contexte d'un bloc.

  Paramètres :
  - i : Instruction à analyser.
  - depl : Décalage initial pour le placement des variables locales.
  - reg : Registre de base pour les adresses mémoire des variables.

  Résultat :
  - Tuple contenant :
    - Instruction avec son placement modifié.
    - Taille de l'instruction, utilisée pour mettre à jour le décalage dans le bloc.
*)
and analyser_placement_instruction i depl reg =
  match i with 
  | AstType.Affectation (info, e) -> 
      (* Analyse le placement d'une affectation, renvoie l'instruction et une taille nulle car elle n'ajoute pas de taille au bloc. *)
      (AstPlacement.Affectation(info, e), 0)

  | AstType.Conditionnelle (c, b1, b2) -> 
      (* Analyse le placement d'une structure conditionnelle. *)
      let nb1 = analyser_placement_bloc b1 depl reg in
      let nb2 = analyser_placement_bloc b2 depl reg in
      (* Renvoie la conditionnelle avec son placement modifié et une taille nulle car elle n'ajoute pas de taille au bloc. *)
      (AstPlacement.Conditionnelle(c, nb1, nb2), 0)

  | AstType.Declaration (info, e) -> 
      (* Analyse le placement d'une déclaration de variable. *)
      (match info_ast_to_info info with
      | InfoVar(_, t, _, _) -> 
          (* Modifie l'adresse de la variable et renvoie la déclaration avec la taille de la variable. *)
          modifier_adresse_variable depl reg info;
          (AstPlacement.Declaration(info, e), getTaille t)

      | _ -> failwith "erreur dans la declaration de variable")

  | AstType.Retour (e, info) -> 
      (* Analyse le placement d'un retour dans une fonction. *)
      (match info_ast_to_info info with
      | InfoFun (_, tr, tp) -> 
          (* Renvoie le retour avec le placement modifié et la taille totale des paramètres de la fonction. *)
          (AstPlacement.Retour(e, getTaille tr, List.fold_right (fun x acc -> getTaille x + acc) tp 0), 0)

      | _ -> failwith "erreur dans la declaration de fonction")

  | AstType.AffichageInt e -> 
      (* Analyse le placement d'un affichage d'entier, renvoie l'instruction et une taille nulle. *)
      (AstPlacement.AffichageInt e, 0)

  | AstType.AffichageRat e -> 
      (* Analyse le placement d'un affichage de nombre rationnel, renvoie l'instruction et une taille nulle. *)
      (AstPlacement.AffichageRat e, 0)

  | AstType.AffichageBool e -> 
      (* Analyse le placement d'un affichage de booléen, renvoie l'instruction et une taille nulle. *)
      (AstPlacement.AffichageBool e, 0)

  | AstType.TantQue (e, b) -> 
      (* Analyse le placement d'une boucle tant que. *)
      let nb = analyser_placement_bloc b depl reg in
      (* Renvoie la boucle tant que avec son placement modifié et une taille nulle. *)
      (AstPlacement.TantQue(e, nb), 0)

  | AstType.Empty -> 
      (* Renvoie une instruction vide avec une taille nulle. *)
      (AstPlacement.Empty, 0)
  | AstType.For (i1, e2, info, e3, b) -> (match (analyser_placement_instruction i1 depl reg) with
                                    | (i, _) -> let nb = analyser_placement_bloc b depl reg in
                                                (AstPlacement.For(i, e2, info, e3, nb), 0))
  | AstType.Goto (s) -> (AstPlacement.Goto(s), 0)
  | AstType.Label (s) -> (AstPlacement.Label(s), 0)

(*
  [analyser_placement_fonction : AstType.fonction -> AstPlacement.fonction]

  Cette fonction analyse le placement des variables locales dans le contexte d'une fonction.

  Paramètre :
  - Fonction à analyser.

  Résultat :
  - Fonction avec les adresses des variables locales modifiées selon leur placement dans la pile.
*)
let analyser_placement_fonction (AstType.Fonction(inf_fonc, inf_param, b)) = 
  let rev = List.rev inf_param in
  let rec mod_all_var l pos = 
    match l with
    | [] -> []
    | t::q -> (match info_ast_to_info t with
              | InfoVar(_, ty, _, _) -> 
                  (* Modifie l'adresse de la variable en fonction de sa taille et renvoie la liste des variables modifiées. *)
                  let taille = getTaille ty in
                  modifier_adresse_variable (pos - taille) "LB" t;
                  mod_all_var q (pos - taille);
              | _ -> failwith "erreur dans la declaration de variable")
  in 
  let _ = mod_all_var rev (0) in 
  AstPlacement.Fonction(inf_fonc, inf_param, analyser_placement_bloc b 3 "LB")
 
(*
  [analyser : AstType.programme -> AstPlacement.programme]

  Cette fonction principale analyse le placement des variables dans le contexte d'un programme.

  Paramètre :
  - Programme à analyser.

  Résultat :
  - Programme avec les adresses des variables modifiées selon leur placement dans la pile.
*)
let analyser (AstType.Programme(fonctions, prog)) = 
  let nlf = List.map analyser_placement_fonction fonctions in
  let nb = analyser_placement_bloc prog 0 "SB" in
  AstPlacement.Programme(nlf, nb)
