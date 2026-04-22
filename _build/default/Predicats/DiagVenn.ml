open Formule_Syllogisme
open Proposition.Formule
(* open Formule_Log_Prop *)

module Predicate_set = Set.Make (String)
(** Module des ensembles de prédicats représentés par des chaines de caractères
*)

(** Type des remplissages possibles d'un diagramme de Venn *)
type fill = Vide | NonVide

module Diag = Map.Make (Predicate_set)
(** Module des Maps dont les clés sont des ensembles de chaines de caractères *)

type diagramme = fill Diag.t
(** Type des diagrammes de Venn *)

(* setstr_to_string e : renvoie un ensemble sous forme de chaîne de caractères *)
let setstr_to_string (e : Predicate_set.t) : string =
  let s = Predicate_set.fold (fun e acc -> e ^ ", " ^ acc) e "" in
  "{" ^ s ^ "}"

(* fill_to_string f : renvoie la valeur de f en une chaine de caractères *)
let fill_to_string (f : fill) : string =
  match f with Vide -> "Vide" | NonVide -> "NonVide"

(** string_of_diag d : conversion d'un diagramme d en une chaine de caractères
*)
let string_of_diag (d : diagramme) : string =
  Diag.fold
    (fun key data acc ->
      setstr_to_string key ^ "->" ^ fill_to_string data ^ "\n" ^ acc)
    d ""

(** diag_from_formule alpha f : construit le diagramme de Venn associé à la
    formule f sur les prédicats issus de f ou de alpha *)
let diag_from_formule (alpha : string list) (f : formule_syllogisme) :
    diagramme list =
  match f with
  | PourTout g ->
      table_verite_with alpha g
      |> List.filter (fun (_, b) -> not b)
      |> List.map (fun (ats, _) -> (Predicate_set.of_list ats, Vide))
      |> fun l -> [ Diag.of_list l ]
  | IlExiste g ->
      table_verite_with alpha g
      |> List.filter (fun (_, b) -> b)
      |> List.map (fun (ats, _) ->
             Diag.of_list [ (Predicate_set.of_list ats, NonVide) ])

(** conj_diag d1 d2 : Calcule la combinaison/conjonction de deux diagrammes,
    renvoyant None si incompatibilité *)
let conj_diag (d1 : diagramme) (d2 : diagramme) : diagramme option =
  Diag.fold
    (fun ats v acc ->
      match acc with
      | None -> None
      | Some d -> (
          match Diag.find_opt ats d with
          | None -> Some (Diag.add ats v d)
          | Some v' when v' = v -> acc
          | _ -> None))
    d1 (Some d2)

(** est_compatible_diag_diag dp dc : teste si le diagramme dp d'une prémisse est
    compatible avec le diagramme dc d'une conclusion *)
let est_compatible_diag_diag (dp : diagramme) (dc : diagramme) : bool =
  Diag.for_all
    (fun ats v ->
      let v' = Diag.find_opt ats dp in
      match v' with None -> false | Some v' -> if v' != v then false else true)
    dc

(** est_compatible_diag_list dp dcs : teste si un diagramme dp d'une prémisse
    est compatible avec un des diagrammes de la liste dcs, diagrammes issus
    d'une conclusion *)
let est_compatible_diag_list (dp : diagramme) (dcs : diagramme list) : bool =
  List.exists (est_compatible_diag_diag dp) dcs

(** est_compatible_list_list dps dcs : teste si chacun des diagrammes de dps,
    diagrammes issus de prémisses, est compatible avec au moins un des
    diagrammes de dcs, diagrammes issus d'une conclusion *)
let est_compatible_list_list (dps : diagramme list) (dcs : diagramme list) :
    bool =
  List.for_all (fun x -> est_compatible_diag_list x dcs) dps

let atoms_from_syllogisme (f : formule_syllogisme) : string list =
  match f with PourTout g -> atomes g | IlExiste g -> atomes g

let atoms_from_syllogisme_list (ps : formule_syllogisme list) : string list =
  List.sort_uniq String.compare
    (List.fold_left (fun acc x -> atoms_from_syllogisme x @ acc) [] ps)

let list_couple (xs : 'a list) (ys : 'b list) : ('a * 'b) list =
  List.concat_map (fun x -> List.map (fun y -> (x, y)) ys) xs

(** conj_diag_list ds1 ds2 renvoie la conjonction de deux listes de diagrammes
    ds1 et ds2 *)
let conj_diag_list (xs : diagramme list) (ys : diagramme list) : diagramme list
    =
  List.concat_map (fun x -> List.filter_map (fun y -> conj_diag x y) ys) xs

(** est_compatible_premisses_conc ps c : teste si une liste de prémisses ps est
    compatible avec une conclusion c *)
let est_compatible_premisses_conc (ps : formule_syllogisme list)
    (c : formule_syllogisme) : bool =
  let atoms =
    List.sort_uniq String.compare
      (atoms_from_syllogisme_list ps @ atoms_from_syllogisme c)
  in
  let dps = List.map (diag_from_formule atoms) ps in
  let dcs = diag_from_formule atoms c in
  let cartesien = List.fold_left conj_diag_list [ Diag.empty ] dps in
  est_compatible_list_list cartesien dcs

(** temoin_incompatibilite_premisses_conc_opt ps c : renvoie un diagramme de la
    combinaison des prémisses ps incompatible avec la conclusion c s'il existe,
    None sinon *)
let temoin_incompatibilite_premisses_conc_opt (ps : formule_syllogisme list)
    (c : formule_syllogisme) : diagramme option =
  let atoms = atoms_from_syllogisme_list ps @ atoms_from_syllogisme c in
  let dps = List.map (diag_from_formule atoms) ps in
  let dcs = diag_from_formule atoms c in
  let cartesien = List.fold_left conj_diag_list [ Diag.empty ] dps in
  List.find_opt (fun x -> est_compatible_diag_list x dcs) cartesien

(** temoins_incompatibilite_premisses_conc ps c : renvoie les diagrammes de la
    combinaison des prémisses ps incompatibles avec la conclusion c *)
let temoins_incompatibilite_premisses_conc (ps : formule_syllogisme list)
    (c : formule_syllogisme) : diagramme list =
  let atoms = atoms_from_syllogisme_list ps @ atoms_from_syllogisme c in
  let dps = List.map (diag_from_formule atoms) ps in
  let dcs = diag_from_formule atoms c in
  let cartesien = List.fold_left conj_diag_list [ Diag.empty ] dps in
  List.fold_left
    (fun acc x ->
      if not (est_compatible_diag_list x dcs) then x :: acc else acc)
    [] cartesien

(* ***** Ajouts pour le projet ***** *)

(** negate_diag d renvoie la négation du diagramme d*)
let negate_diag (d : diagramme) : diagramme list =
  Diag.fold
    (fun at v acc ->
      match v with
      | Vide -> Diag.add at NonVide Diag.empty :: acc
      | NonVide -> Diag.add at Vide Diag.empty :: acc)
    d []

(** VOIR LE DIAGRAMME COMME UNE CONJONCTION DE CONTRAINTE *)

(** negate_diag_list ds renvoie la négation de la liste de diagrammes ds *)

(*
let negate_diag_list (ds : diagramme list) : diagramme list =
  conj_diag_list [ Diag.empty ] (List.concat_map (fun d -> negate_diag d) ds)
*)

let negate_diag_list (ds : diagramme list) : diagramme list =
  List.fold_left
    (fun acc d ->
      let neg_d = negate_diag d in
      (* nouvelle liste de résultats *)
      List.concat_map
        (fun d1 -> List.filter_map (fun d2 -> conj_diag d1 d2) neg_d)
        acc)
    [ Diag.empty ] ds

(** disj_of_diag_list ds1 ds2 renvoie la disjonction de deux listes de
    diagrammes ds1 et ds2 *)
let disj_of_diag_list (ds1 : diagramme list) (ds2 : diagramme list) :
    diagramme list =
  ds1 @ ds2

(** diags_of_bool_comb alpha b renvoie la liste des diagrammes associés à la
    combinaison booléenne b de formules pour syllogismes, sur les prédicats
    issus de b ou de alpha *)
let rec diags_of_bool_comb (alpha : string list) (b : boolCombSyllogismes) :
    diagramme list =
  match b with
  | Vrai -> [ Diag.empty ]
  | Faux -> []
  | Base g -> diag_from_formule alpha g
  | Et (g1, g2) ->
      conj_diag_list (diags_of_bool_comb alpha g1) (diags_of_bool_comb alpha g2)
  | Ou (g1, g2) ->
      disj_of_diag_list
        (diags_of_bool_comb alpha g1)
        (diags_of_bool_comb alpha g2)
  | Non g -> negate_diag_list (diags_of_bool_comb alpha g)
