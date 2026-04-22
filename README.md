# Projet PREDICAT - Logique et Syllogismes

## Description
Ce projet codé en OCaml est dédié à la manipulation de formules de logique propositionnelle et à l'évaluation de la validité de syllogismes en utilisant la méthode des diagrammes de Venn. Il propose deux algorithmes pour vérifier la validité d'une conclusion à partir de prémisses (naïve et par la négation) ainsi qu'un système complet de tests aléatoires pour s'assurer de leur cohérence.

## Fonctionnalités Principales
* **Logique Propositionnelle :** Création d'arbres syntaxiques de formules (Atomes, ET, OU, IMPLIQUE, NON), évaluation avec des interprétations, et génération de tables de vérité.
* **Syllogismes :** Support des quantificateurs `PourTout` (∀) et `IlExiste` (∃), et de leurs combinaisons booléennes complexes.
* **Diagrammes de Venn :** Représentation formelle sous forme de map d'ensembles, permettant de combiner des diagrammes, faire leur négation et vérifier la compatibilité entre des prémisses et des conclusions.
* **Tests de Validité :** * *Méthode naïve* : extension complète des diagrammes pour chercher des zones de contradiction.
  * *Méthode par la négation* : vérification via la conjonction des diagrammes des prémisses avec la négation de la conclusion.
* **Génération de Tests Aléatoires :** Génération automatique de syllogismes aléatoires pour comparer massivement les deux méthodes de validité et vérifier leur équivalence.

## Structure du Code
Le projet est divisé en deux modules principaux construits avec Dune :

* **`Proposition/`**
  * `Formule.ml` : Définition des types de base de la logique propositionnelle et algorithmes d'évaluation.
* **`Predicats/`**
  * `Formule_Syllogisme.ml` : Types et affichage des syllogismes avec quantificateurs.
  * `DiagVenn.ml` : Logique de manipulation des diagrammes de Venn (intersections, unions, listes de diagrammes).
  * `ValiditeNaive.ml` : Implémentation de la vérification de validité de manière naïve.
  * `ValiditeViaNegation.ml` : Implémentation optimisée par la négation.
  * `randomSyllogisme.ml` : Générateur aléatoire d'atomes, de formules et de syllogismes.
  * `Test.ml` : Script automatisé avec un affichage détaillé dans la console des résultats et incohérences trouvées.

## Prérequis et Compilation

* Pour compiler l'intégralité du projet :
  ```bash
  dune build
