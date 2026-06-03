(ns katzen.cat-test
  "The generic category interface `katzen.cat` (compose/id/dom/codom) must
   route to the same per-category law as the named functions, across
   FinSet, ACSet morphisms, and theory morphisms; and `katzen.migrate`
   must route Δ-migration by what is migrated."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.acset :as a]
            [katzen.acset.migration :as amig]
            [katzen.acset.morphism :as am]
            [katzen.cat :as cat]
            [katzen.finset :as fs]
            [katzen.migrate :as kmig]
            [katzen.morphism :as tm]
            [katzen.stdlib.core :as stdlib]))

(deftest cat-over-finset
  (let [f (fs/fin-function [1 2] 3)    ; 2 → 3
        g (fs/fin-function [0 0 1] 2)] ; 3 → 2
    (is (= (:vals (cat/compose f g)) (:vals (fs/compose f g)))
        "compose routes to fs/compose (diagrammatic: first f then g)")
    (is (= 2 (:n (cat/dom f))))
    (is (= 3 (:n (cat/codom f))))
    (is (= [0 1 2] (:vals (cat/id (fs/fin-set 3)))))))

(deftest cat-over-theory-maps
  (let [idm (tm/id-theory-map stdlib/ThMonoid)]
    (is (= idm (cat/compose idm idm))   "id ∘ id = id, via cat/compose → compose-morphisms")
    (is (= stdlib/ThMonoid (cat/dom idm)))
    (is (= stdlib/ThMonoid (cat/codom idm)))
    (is (= idm (cat/id stdlib/ThMonoid)) "cat/id on a GAT builds its identity theory map")))

(deftest cat-over-acset-morphisms
  (let [sch {:name 'V :objects [:V] :homs [] :attr-types [] :attrs []}
        x   (-> (a/vector-acset sch) (a/add-parts :V 2) first)
        idx (am/identity-morphism x)]
    (is (= idx (cat/compose idx idx)) "id ∘ id = id, via cat/compose → am/compose")
    (is (= x (cat/dom idx)))
    (is (= x (cat/codom idx)))
    (is (= idx (cat/id x)) "cat/id on an ACSet builds its identity morphism")))

(deftest migrate-routes-by-target
  (testing "the unified migrate dispatches on what is being migrated"
    (let [sch {:name 'OneObj :objects [:V] :homs [] :attr-types [] :attrs []}
          x   (-> (a/vector-acset sch) (a/add-parts :V 3) first)
          F   (amig/schema-morphism 'Id sch sch {:V :V} {})]
      (is (= (count (a/parts (kmig/migrate F x) :V))
             (count (a/parts (amig/migrate F x) :V)))
          ":acset route == acset.migration/migrate")
      (is (= #{:acset :morphism :dynamics} (set (keys (methods kmig/migrate))))
          "all three migration targets registered")
      (is (thrown-with-msg? Exception #"unrecognized migration target"
                            (kmig/migrate F 42))))))
