open Formule_Syllogisme
open Proposition.Formule

(** rdm_at renvoie un atome aléatoire de al*)
let rdm_at (alist : string list) : formule =
  let n = Random.int (List.length alist) in
  Atome (List.nth alist n)

(** rdm_formule atoms n renvoie une formule aléatoire possédant n opérateurs et
    des atomes parmi atoms. *)
let rec rdm_formule (atoms : string list) (n : int) : formule =
  match n with
  | 0 -> rdm_at atoms
  | 1 -> (
      match Random.int 5 with
      | 0 -> Bot
      | 1 -> Top
      | 2 -> Non (rdm_at atoms)
      | 3 -> Imp (rdm_at atoms, rdm_at atoms)
      | _ -> Ou (rdm_at atoms, rdm_at atoms))
  | _ -> (
      let k = Random.int (n - 1) in
      match Random.int 4 with
      | 0 -> Imp (rdm_formule atoms k, rdm_formule atoms (n - k - 1))
      | 1 -> Ou (rdm_formule atoms k, rdm_formule atoms (n - k - 1))
      | 2 -> Et (rdm_formule atoms k, rdm_formule atoms (n - k - 1))
      | _ -> Non (rdm_formule atoms (n - 1)))

(** rdm_syllogisme_frm : renvoie un syllogisme simple aléatoire (PourTout ou
    IlExiste) à partir d'une formule *)
let rdm_syllogisme_frm (f : formule) : formule_syllogisme =
  match Random.int 2 with 0 -> PourTout f | _ -> IlExiste f

(** rdm_syllogisme_simple : génère un syllogisme simple aléatoire avec n
    opérateurs *)
let rdm_syllogisme_simple (atoms : string list) (n : int) : formule_syllogisme =
  rdm_syllogisme_frm (rdm_formule atoms n)

(** rdm_comp_syllogisme atoms n renvoie un syllogisme complexe aléatoire
    possédant environ n nœuds dans l'arbre de combinaisons booléennes *)
let rec rdm_comp_syllogisme (atoms : string list) (n : int) :
    boolCombSyllogismes =
  match n with
  | 0 -> (
      match Random.int 3 with
      | 0 -> Vrai
      | 1 -> Faux
      | _ -> Base (rdm_syllogisme_simple atoms (Random.int 3)))
  | 1 -> Base (rdm_syllogisme_simple atoms (Random.int 4))
  | _ -> (
      let k = Random.int (n - 1) in
      match Random.int 4 with
      | 0 ->
          Et (rdm_comp_syllogisme atoms k, rdm_comp_syllogisme atoms (n - k - 1))
      | 1 ->
          Ou (rdm_comp_syllogisme atoms k, rdm_comp_syllogisme atoms (n - k - 1))
      | 2 -> Non (rdm_comp_syllogisme atoms (n - 1))
      | _ -> Base (rdm_syllogisme_simple atoms (Random.int (n + 2))))

(** rdm_syllogisme : génère un syllogisme complexe avec des paramètres par
    défaut *)
let rdm_syllogisme ?(atoms = [ "A"; "B"; "C" ]) ?(complexity = 3) () :
    boolCombSyllogismes =
  rdm_comp_syllogisme atoms complexity

(** rdm_syllogisme_liste : génère une liste de n syllogismes aléatoires *)
let rdm_syllogisme_liste (atoms : string list) (complexity : int) (count : int)
    : boolCombSyllogismes list =
  let rec aux acc n =
    if n <= 0 then acc
    else aux (rdm_comp_syllogisme atoms complexity :: acc) (n - 1)
  in
  aux [] count
