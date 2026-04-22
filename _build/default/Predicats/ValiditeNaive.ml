open DiagVenn
open Formule_Syllogisme
open Proposition.Formule

let all_possibility (ats : string list) = all_sublists ats

(** complete_diags d ats renvoie la liste des extensions complètes de d en
    considérant les atomes de la liste ats pour considérer les zones à
    compléter. Par exemple, un diagramme défini par une contrainte vide sur une
    zone A pourrait être complété en considérant les zones définies par une
    liste d'atomes [A; B; C]. *)
let complete_diags (d : diagramme) (ats : string list) : diagramme list =
  let sublist = all_possibility ats in
  let predicate = List.map (fun x -> Predicate_set.of_list x) sublist in
  let rec aux dl p =
    match p with
    | [] -> [ dl ]
    | x :: xs -> (
        match Diag.find_opt x d with
        | None -> aux (Diag.add x Vide dl) xs @ aux (Diag.add x NonVide dl) xs
        | Some _ -> aux dl xs)
  in
  aux d predicate

(* Faire la liste des premisses du diagramme et retiré les atomes qui sont dans le diagramme et dans ats pour ne pas modifier ( il y a déjà une contrainte )*)

(** is_contradiction d1 d2 teste si les diagrammes d1 et d2 sont en
    contradiction, c'est-à-dire s'il existe une zone non-vide de d1 qui est vide
    dans d2 ou inversement *)

let is_contradiction (d1 : diagramme) (d2 : diagramme) : bool =
  Diag.exists
    (fun at v ->
      match Diag.find_opt at d2 with None -> false | Some v' -> v' != v)
    d1

let atoms_from_syllogisme (f : formule_syllogisme) : string list =
  match f with PourTout g -> atomes g | IlExiste g -> atomes g

let rec atoms_from_boolCombSyllogisme (b : boolCombSyllogismes) : string list =
  match b with
  | Vrai | Faux -> []
  | Base g -> atoms_from_syllogisme g
  | Et (g1, g2) ->
      List.sort_uniq String.compare
        (atoms_from_boolCombSyllogisme g1 @ atoms_from_boolCombSyllogisme g2)
  | Ou (g1, g2) ->
      List.sort_uniq String.compare
        (atoms_from_boolCombSyllogisme g1 @ atoms_from_boolCombSyllogisme g2)
  | Non g1 -> atoms_from_boolCombSyllogisme g1

(** est_valid_premiss_conc b1 b2 teste si pour deux combinaisons booléennes de
    formules pour syllogismes b1 et b2, b1 valide b2*)

let est_valid_premiss_conc (b1 : boolCombSyllogismes) (b2 : boolCombSyllogismes)
    : bool =
  let atoms =
    List.sort_uniq String.compare
      (atoms_from_boolCombSyllogisme b1 @ atoms_from_boolCombSyllogisme b2)
  in
  let dps = diags_of_bool_comb atoms b1 in
  let dcs = diags_of_bool_comb atoms b2 in
  not
    (List.exists
       (fun d ->
         let extensions_d = complete_diags d atoms in
         List.exists
           (fun de ->
             List.for_all (fun d2_diag -> is_contradiction de d2_diag) dcs)
           extensions_d)
       dps)

(** temoins_invalidite_premisses_conc b1 b2 renvoie les diagrammes de la
    combinaison des prémisses b1 invalidant la conclusion b2 *)

let temoins_invalidite_premisses_conc (b1 : boolCombSyllogismes)
    (b2 : boolCombSyllogismes) : diagramme list =
  let atoms =
    List.sort_uniq String.compare
      (atoms_from_boolCombSyllogisme b1 @ atoms_from_boolCombSyllogisme b2)
  in
  let dps = diags_of_bool_comb atoms b1 in
  let dcs = diags_of_bool_comb atoms b2 in
  List.fold_left
    (fun acc d ->
      let extensions_d = complete_diags d atoms in
      let temoins_pour_d =
        List.filter
          (fun de -> List.for_all (fun dc -> is_contradiction de dc) dcs)
          extensions_d
      in
      match temoins_pour_d with [] -> acc | _ -> d :: acc)
    [] dps
