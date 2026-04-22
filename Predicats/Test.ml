(* ========================================
   TESTS ALÉATOIRES DE COHÉRENCE
   Tests automatiques avec syllogismes générés
   ======================================== *)

open Formule_Syllogisme
open ValiditeNaive
open ValiditeViaNegation
open RandomSyllogisme

(* ========================================
   FONCTIONS D'AFFICHAGE
   ======================================== *)

(** Affiche une combinaison booléenne de syllogismes *)
let rec string_of_boolCombSyllogismes (b : boolCombSyllogismes) : string =
  match b with
  | Vrai -> "⊤"
  | Faux -> "⊥"
  | Base f -> string_of_formule_syllogisme f
  | Et (b1, b2) ->
      "("
      ^ string_of_boolCombSyllogismes b1
      ^ " ∧ "
      ^ string_of_boolCombSyllogismes b2
      ^ ")"
  | Ou (b1, b2) ->
      "("
      ^ string_of_boolCombSyllogismes b1
      ^ " ∨ "
      ^ string_of_boolCombSyllogismes b2
      ^ ")"
  | Non b1 -> "¬(" ^ string_of_boolCombSyllogismes b1 ^ ")"

(* ========================================
   TEST DE COHÉRENCE ENTRE LES DEUX MÉTHODES
   ======================================== *)

(** Teste la cohérence entre les deux méthodes pour un couple (prémisses,
    conclusion) *)
let test_coherence premisses conclusion =
  let valide_naive = est_valid_premiss_conc premisses conclusion in
  let valide_negation = est_valid_premiss_conc' premisses conclusion in

  if valide_naive = valide_negation then (true, valide_naive)
    (* cohérent, + résultat *)
  else (false, valide_naive)
(* incohérent *)

(** Teste un couple de syllogismes avec affichage détaillé *)
let test_syllogisme_detail num premisses conclusion =
  Printf.printf "\n========================================\n";
  Printf.printf "TEST %d\n" num;
  Printf.printf "========================================\n";

  (* Affichage des formules *)
  Printf.printf "Prémisses:\n";
  Printf.printf "  %s\n" (string_of_boolCombSyllogismes premisses);
  Printf.printf "\nConclusion:\n";
  Printf.printf "  %s\n" (string_of_boolCombSyllogismes conclusion);
  Printf.printf "\n";

  let valide_naive = est_valid_premiss_conc premisses conclusion in
  Printf.printf "Validité (méthode naïve)    : %b\n" valide_naive;

  let valide_negation = est_valid_premiss_conc' premisses conclusion in
  Printf.printf "Validité (méthode négation) : %b\n" valide_negation;

  if valide_naive = valide_negation then (
    Printf.printf "\n✓ COHÉRENT";
    if valide_naive then Printf.printf " - Le syllogisme est VALIDE\n"
    else Printf.printf " - Le syllogisme est INVALIDE\n";
    (true, valide_naive))
  else (
    Printf.printf "\n✗ INCOHÉRENT !!!\n";
    Printf.printf "⚠️  Les deux méthodes donnent des résultats différents !\n";
    (false, valide_naive))

(* ========================================
   TESTS ALÉATOIRES MASSIFS
   ======================================== *)

(** Génère et teste N couples de syllogismes aléatoires *)
let test_aleatoires nb_tests atoms complexity =
  Printf.printf "\n╔════════════════════════════════════════╗\n";
  Printf.printf "║  TESTS ALÉATOIRES DE COHÉRENCE         ║\n";
  Printf.printf "╚════════════════════════════════════════╝\n";
  Printf.printf "Configuration:\n";
  Printf.printf "  - Nombre de tests : %d\n" nb_tests;
  Printf.printf "  - Atomes          : [%s]\n" (String.concat "; " atoms);
  Printf.printf "  - Complexité      : %d\n" complexity;
  Printf.printf "========================================\n";

  Random.self_init ();

  let rec aux n acc_coherent acc_incoherent acc_valides =
    if n <= 0 then (acc_coherent, acc_incoherent, acc_valides)
    else
      (* Génère une prémisse et une conclusion aléatoires *)
      let premisses = rdm_comp_syllogisme atoms complexity in
      let conclusion = rdm_comp_syllogisme atoms complexity in

      (* Teste la cohérence *)
      let coherent, valide = test_coherence premisses conclusion in

      (* Affiche un point tous les 10 tests *)
      if n mod 10 = 0 then Printf.printf ".%!";

      (* Si incohérence détectée, affiche les détails *)
      if not coherent then (
        Printf.printf "\n\n⚠️  INCOHÉRENCE DÉTECTÉE au test %d !\n"
          (nb_tests - n + 1);
        let _ =
          test_syllogisme_detail (nb_tests - n + 1) premisses conclusion
        in
        ());

      aux (n - 1)
        (if coherent then acc_coherent + 1 else acc_coherent)
        (if not coherent then acc_incoherent + 1 else acc_incoherent)
        (if valide then acc_valides + 1 else acc_valides)
  in

  let coherents, incoherents, valides = aux nb_tests 0 0 0 in

  Printf.printf "\n\n========================================\n";
  Printf.printf "RÉSULTATS:\n";
  Printf.printf "========================================\n";
  Printf.printf "Tests effectués      : %d\n" nb_tests;
  Printf.printf "Tests cohérents      : %d (%.1f%%)\n" coherents
    (100.0 *. float_of_int coherents /. float_of_int nb_tests);
  Printf.printf "Tests incohérents    : %d (%.1f%%)\n" incoherents
    (100.0 *. float_of_int incoherents /. float_of_int nb_tests);
  Printf.printf "Syllogismes valides  : %d (%.1f%%)\n" valides
    (100.0 *. float_of_int valides /. float_of_int nb_tests);
  Printf.printf "========================================\n";

  if incoherents = 0 then
    Printf.printf "✓ TOUTES LES MÉTHODES SONT COHÉRENTES !\n"
  else Printf.printf "✗ DES INCOHÉRENCES ONT ÉTÉ DÉTECTÉES !\n";

  Printf.printf "========================================\n\n"

(* ========================================
   TESTS AVEC AFFICHAGE DÉTAILLÉ
   ======================================== *)

(** Teste N syllogismes avec affichage détaillé de chacun *)
let test_aleatoires_detailles nb_tests atoms complexity =
  Printf.printf "\n╔════════════════════════════════════════╗\n";
  Printf.printf "║  TESTS DÉTAILLÉS                       ║\n";
  Printf.printf "╚════════════════════════════════════════╝\n";

  Random.self_init ();

  let rec aux n acc_coherent acc_incoherent acc_valides =
    if n <= 0 then (acc_coherent, acc_incoherent, acc_valides)
    else
      let premisses = rdm_comp_syllogisme atoms complexity in
      let conclusion = rdm_comp_syllogisme atoms complexity in

      let coherent, valide =
        test_syllogisme_detail (nb_tests - n + 1) premisses conclusion
      in

      aux (n - 1)
        (if coherent then acc_coherent + 1 else acc_coherent)
        (if not coherent then acc_incoherent + 1 else acc_incoherent)
        (if valide then acc_valides + 1 else acc_valides)
  in

  let coherents, incoherents, valides = aux nb_tests 0 0 0 in

  Printf.printf "\n========================================\n";
  Printf.printf "RÉSUMÉ FINAL:\n";
  Printf.printf "========================================\n";
  Printf.printf "Tests effectués      : %d\n" nb_tests;
  Printf.printf "Cohérents            : %d (%.1f%%)\n" coherents
    (100.0 *. float_of_int coherents /. float_of_int nb_tests);
  Printf.printf "Incohérents          : %d (%.1f%%)\n" incoherents
    (100.0 *. float_of_int incoherents /. float_of_int nb_tests);
  Printf.printf "Syllogismes valides  : %d (%.1f%%)\n" valides
    (100.0 *. float_of_int valides /. float_of_int nb_tests);
  Printf.printf "========================================\n\n"

(* ========================================
   EXEMPLES D'UTILISATION
   ======================================== *)

(* Test rapide : 100 tests, 2 atomes, complexité 2 *)
let test_rapide () = test_aleatoires 20 [ "a"; "b" ] 2

(* Test moyen : 500 tests, 3 atomes, complexité 3 *)
let test_moyen () = test_aleatoires 20 [ "a"; "b"; "c" ] 3

(* Test intensif : 1000 tests, 3 atomes, complexité 4 *)
let test_intensif () = test_aleatoires 20 [ "a"; "b"; "c" ] 4

(* Test avec affichage détaillé (seulement quelques tests) *)
let test_detaille () = test_aleatoires_detailles 10 [ "a"; "b"; "c" ] 2

(* ========================================
   LANCEMENT DES TESTS
   ======================================== *)

(* Décommentez la ligne que vous voulez exécuter : *)

(* Test avec détails (recommandé pour voir les formules) *)
(* let () = test_detaille () *)

(* Test personnalisé AVEC AFFICHAGE DES FORMULES *)
let () = test_aleatoires_detailles 20 [ "a"; "b"; "c" ] 3

(* Test rapide SANS affichage des formules (juste stats) *)
(* let () = test_rapide () *)

(* Test moyen SANS affichage *)
(* let () = test_moyen () *)

(* Test intensif SANS affichage *)
(* let () = test_intensif () *)
