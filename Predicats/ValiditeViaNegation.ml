open DiagVenn
open Formule_Syllogisme
open ValiditeNaive

(** est_valid_premiss_conc b1 b2 teste si pour deux combinaisons booléennes de
    formules pour syllogismes b1 et b2, b1 valide b2, en utilisant la méthode de
    la conjonction avec les diagrammes inverses de la conclusion *)
let est_valid_premiss_conc' (b1 : boolCombSyllogismes)
    (b2 : boolCombSyllogismes) : bool =
  let atoms =
    List.sort_uniq String.compare
      (atoms_from_boolCombSyllogisme b1 @ atoms_from_boolCombSyllogisme b2)
  in
  let dps = diags_of_bool_comb atoms b1 in
  let dcs = diags_of_bool_comb atoms b2 in
  let conj_dps_neg_dcs = conj_diag_list dps (negate_diag_list dcs) in
  match conj_dps_neg_dcs with [] -> true | _ -> false

(** temoins_invalidite_premisses_conc' b1 b2 renvoie les diagrammes de la
    conjonction des diagrammes de b1 avec la négation de b2, qui contredisent
    chaque diagramme de b2 *)

let temoins_invalidite_premisses_conc' (b1 : boolCombSyllogismes)
    (b2 : boolCombSyllogismes) : diagramme list =
  let atoms =
    List.sort_uniq String.compare
      (atoms_from_boolCombSyllogisme b1 @ atoms_from_boolCombSyllogisme b2)
  in
  let dps = diags_of_bool_comb atoms b1 in
  let dcs = diags_of_bool_comb atoms b2 in
  conj_diag_list dps (negate_diag_list dcs)
