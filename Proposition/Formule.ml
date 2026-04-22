(** Le module Formule contient les types et définitions de base permettant la
    manipulation des formules de la logique propositionnelle. *)

(** Type des formules de la logique propositionnelle, avec des string comme
    atomes. *)
type formule =
  | Bot
  | Top
  | Atome of string
  | Imp of (formule * formule)
  | Ou of (formule * formule)
  | Et of (formule * formule)
  | Non of formule

(* ----------------- Exercice 1 : Hauteur ----------------- *)

(** Calcule la hauteur de l'arbre syntaxique d'une formule. *)
let rec hauteur (f : formule) : int =
  match f with
  | Bot | Top | Atome _ -> 1
  | Imp (f1, f2) -> 1 + max (hauteur f1) (hauteur f2)
  | Ou (f1, f2) -> 1 + max (hauteur f1) (hauteur f2)
  | Et (f1, f2) -> 1 + max (hauteur f1) (hauteur f2)
  | Non f1 -> 1 + hauteur f1

(* ----------------- Exercice 2 : Représentation en chaîne de caractères ----------------- *)

(** Conversion d'une formule en chaîne de caractères. *)
let rec string_of_formule (f : formule) : string =
  match f with
  | Bot -> "⊥"
  | Top -> "⊤"
  | Atome s -> s
  | Non g -> "Non" ^ "(" ^ string_of_formule g ^ ")"
  | Ou (g1, g2) -> string_of_formule g1 ^ " ∨ " ^ string_of_formule g2
  | Et (g1, g2) -> string_of_formule g1 ^ " ∧ " ^ string_of_formule g2
  | Imp (g1, g2) -> string_of_formule g1 ^ " → " ^ string_of_formule g2

(* ----------------- Exercice 3 : Conversion depuis une liste ----------------- *)

(** Transforme une liste de formules [[f1; f2; ... ; fl]] en la formule
    [f1 ∧ f2 ∧ ... ∧ fl] en considérant les éléments suivants : Si un des [fi]
    vaut [Bot], renvoie [Bot]. Si un des [fi] vaut [Top], il n'apparait pas dans
    le résultat. Si tous les [fi] valent [Top], renvoie [Top]. *)
let rec conj_of_list (f : formule list) : formule =
  match f with
  | [] -> Top
  | x :: xs -> (
      match x with Top -> conj_of_list xs | Bot -> Bot | _ -> conj_of_list xs)

(** Transforme une liste de formules [[f1; f2; ... ; fl]] en la formule
    [f1 ∨ f2 ∨ ... ∨ fl] en considérant les éléments suivants : Si un des [fi]
    vaut [Top], renvoie [Top]. Si un des [fi] vaut [Bot], il n'apparait pas dans
    le résultat. Si tous les [fi] valent [Bot], renvoie [Bot]. *)
let rec disj_of_list (f : formule list) : formule =
  match f with
  | [] -> Top
  | x :: xs -> (
      match x with
      | Top -> Top
      | Bot -> disj_of_list xs
      | _ -> Ou (x, disj_of_list xs))

(** --- Exercice 4 : Fonctions d'évaluation ------- *)

type interpretation = string -> bool
(** Type des interprétations. *)

(** Évalue une formule en fonction d'une interprétation. *)
let rec eval (i : interpretation) (f : formule) : bool =
  match f with
  | Top -> true
  | Bot -> false
  | Atome a -> i a
  | Imp (f1, f2) -> eval i f1 <= eval i f2
  | Et (f1, f2) -> eval i f1 && eval i f2
  | Ou (f1, f2) -> eval i f1 || eval i f2
  | Non f1 -> not (eval i f1)

(** --- Exercice 5 : Tests de satisfaisabilité ------- *)

(** Transforme une liste de string en une interprétation. *)
let interpretation_of_list (xs : string list) : interpretation =
 fun s -> List.mem s xs

(** Calcule la liste de toutes les sous-listes d'une liste donnée. *)
let rec all_sublists (l : 'a list) : 'a list list =
  match l with
  | [] -> [ [] ]
  | x :: xs ->
      let m = all_sublists xs in
      List.map (fun l -> x :: l) m @ m

(** Calcule toutes les interprétations pour une liste d'atomes donnée. *)
let all_interpretations (_ : string list) : interpretation list =
  failwith "a faire"

(** Calcule la liste (triée et sans doublon) des atomes d'une formule.*)
let atomes (f : formule) : string list =
  let rec atom_list = function
    | Bot | Top -> []
    | Atome a -> [ a ]
    | Imp (f1, f2) | Ou (f1, f2) | Et (f1, f2) -> atom_list f1 @ atom_list f2
    | Non f1 -> atom_list f1
  in
  List.sort_uniq String.compare (atom_list f)

(** Détermine si une formule est satisfaisable. *)
let est_satisfaisable (_ : formule) : bool = failwith "à faire"

(** Renvoie un témoin de la satisfaisabilité d'une formule, s'il en existe. *)
let ex_sat (_ : formule) : interpretation option = failwith "à faire"

(** Détermine si une formule est une tautologie. *)
let est_tautologie (_ : formule) : bool = failwith "à faire"

(** Détermine si une formule est une contradiction. *)
let est_contradiction (_ : formule) : bool = failwith "à faire"

(** Détermine si une formule est contingente. *)
let est_contingente (_ : formule) : bool = failwith "à faire"

(** ----------------- Exercice 8 : Tables de vérité ----------------- *)

type ligne = string list * bool
(** Type d'une ligne d'une table de vérité. *)

type table = ligne list
(** Type d'une table de vérité. *)

(** Calcule la table de vérité associée à une formule. *)
let table_of_formule (f : formule) : table =
  let ats = atomes f in
  let xss = all_sublists ats in
  List.map (fun xs -> (xs, eval (interpretation_of_list xs) f)) xss

(** Calcule la table de vérité associée à une formule sur l'alphabet alpha. *)
let table_verite_with (alpha : string list) (f : formule) : table =
  let ats = List.sort_uniq String.compare (alpha @ atomes f) in
  let xss = all_sublists ats in
  List.map (fun xs -> (xs, eval (interpretation_of_list xs) f)) xss
