(* Module de la passe de gestion des identifiants *)
(* doit être conforme à l'interface Passe *)
open Ast
open Tds
open Tam
open Type
open Code


type t1 = Ast.AstPlacement.programme
type t2 = string


(*
  [analyse_code_affectable : AstType.affectable -> string -> bool -> string]

  Cette fonction génère le code assembleur pour un affectable de type AstType.

  Paramètres :
  - a : Affectable AstType à générer.
  - mode : Mode d'accès ("l" pour lecture, "e" pour écriture).
  - deref : Booléen indiquant si l'affectable a subi un déréférencement.

  Résultat :
  - Chaîne de caractères représentant le code assembleur généré.
*)

let rec analyser_code_affectable a mode deref =
  (* Analyse de l'affectable *)
  match a with
  | AstType.Ident info ->
    (match info_ast_to_info info with
     | InfoVar(_, t, d, reg) -> 
        if est_compatible t (Pointer(Undefined)) then
          if mode = "e" && not deref then
            store (getTaille t) d reg
          else
            load (getTaille t) d reg
        else if mode = "l" then
          load (getTaille t) d reg
        else
          store (getTaille t) d reg
     | InfoConst(_, i) -> loadl_int i
     | _ -> failwith "Impossible Ident")
  | AstType.Deref (da, t) -> analyser_code_affectable da "e" true
  (* Si on est en écriture alors écrit à l'adresse en haut de la pile *)
  ^ (if (mode == "e" && not deref) then storei (getTaille(t))
  (* Sinon charge la valeur pointée par l'adresse en haut de la pile *)
  else loadi (getTaille(t)))
  | AstType.TabInd (a, e, t) -> 
    let res = analyser_code_affectable a "e" false in
    let res2 = analyser_code_expression e in
    res ^ res2 ^ loadl_int (getTaille (t)) ^ subr "IMul" ^ subr "IAdd" ^
    (if mode = "e" && not deref then
       storei (getTaille (t))
     else
       loadi (getTaille (t)))
    

and analyser_code_expression e = 
  match e with
  | AstType.AppelFonction (info, l) ->  (match info_ast_to_info info with
                                          | InfoFun(nom,_,_) ->  String.concat "" (List.map analyser_code_expression l) ^
                                                                 call "ST" nom
                                          | _ -> failwith "Impossible")

  | AstType.Entier(i) -> loadl_int i
  | AstType.Booleen(b) -> (match b with
                              | true -> loadl_int 1
                              | false -> loadl_int 0 )
  | AstType.Affectable(a) -> analyser_code_affectable a "l" false
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
  | AstType.Null -> loadl_int 0
  | AstType.New(t) -> loadl_int (getTaille t) ^ subr "MAlloc"
  | AstType.Adresse(infoa) -> (match info_ast_to_info infoa with
                                | InfoVar(_,_,d,reg) -> loada  d reg
                                | _ -> failwith "Impossible")
  | AstType.ListeValeurs(l) -> String.concat "" (List.map analyser_code_expression l)
  | AstType.NewTab(t, e) -> let res = analyser_code_expression e in
                            res ^ loadl_int (getTaille t) ^ subr "IMul" ^ subr "MAlloc"


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
  | AstPlacement.Affectation (a,expr) ->  analyser_code_expression expr ^ analyser_code_affectable a "e" false
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
  | AstPlacement.For (i1, e2, a, e3, b) -> let debut = getEtiquette() in
                                            let fin = getEtiquette() in
                                            analyser_code_instruction i1 ^
                                            label debut ^
                                            analyser_code_expression e2 ^
                                            jumpif 0 fin ^
                                            analyser_code_bloc b ^
                                            analyser_code_expression e3 ^
                                            analyser_code_affectable a "e" false ^
                                            jump debut ^
                                            label fin
  | AstPlacement.Goto (is) -> (match info_ast_to_info is with
                              | InfoEtiq (_, _, _, etiq) -> jump etiq
                              | _ -> failwith "Impossible" )
  | AstPlacement.Label (is) -> (match info_ast_to_info is with
                              | InfoEtiq (_, _, _, etiq) -> label etiq
                              | _ -> failwith "Impossible" )



let analyser_code_fonction (AstPlacement.Fonction(inf_fonc, _, b)) =
  match info_ast_to_info inf_fonc with
  | InfoFun (nom,_,_) -> label nom ^ analyser_code_bloc b ^ halt
  | _ -> failwith "Impossible"
 

let analyser (AstPlacement.Programme(fonctions, prog)) = 
  let foncs = String.concat "" (List.map analyser_code_fonction fonctions) in
  let code = ("main \n" ^ analyser_code_bloc prog ^ halt ) in
  let analys = getEntete() ^ foncs ^ code in
  print_endline analys;
  analys