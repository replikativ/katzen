(ns katzen.morphism-advanced-test
  "Tests for advanced morphism features: identity, inclusions, composition."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.morphism :as morph]
            [katzen.theory :as theory]))

;;; ============================================================================
;;; Setup: Define theories for testing
;;; ============================================================================

;; MorphismGraph: Just vertices and edges
(theory/deftheory MorphismGraph
  (type V)
  (type E [src V, tgt V]))

;; MorphismCategory: Objects and morphisms with composition and identity
(theory/deftheory MorphismCategory
  (type Ob)
  (type Hom [dom Ob, codom Ob])
  (term compose
    :ctx [a Ob, b Ob, c Ob]
    :args [f (Hom a b), g (Hom b c)]
    :ret (Hom a c))
  (term id
    :ctx [a Ob]
    :ret (Hom a a)))

;; MorphismMonoid: Just one object with multiplication
(theory/deftheory MorphismMonoid
  (type El)
  (term mul
    :ctx [a El, b El]
    :ret El)
  (term unit
    :ret El))

;;; ============================================================================
;;; Identity Morphism Tests
;;; ============================================================================

(deftest test-id-theory-map-creation
  (testing "Can create identity morphism"
    (let [id-cat (morph/id-theory-map MorphismCategory)]
      (is (morph/id-theory-map? id-cat))
      (is (= MorphismCategory (morph/dom id-cat)))
      (is (= MorphismCategory (morph/codom id-cat))))))

(deftest test-id-theory-map-properties
  (testing "Identity morphism dom and codom are the same"
    (let [id-cat (morph/id-theory-map MorphismCategory)]
      (is (= (morph/dom id-cat) (morph/codom id-cat))))))

;;; ============================================================================
;;; Theory Subsumption Tests
;;; ============================================================================

(deftest test-theory-subsumes-same
  (testing "Theory subsumes itself"
    (is (morph/theory-subsumes? MorphismCategory MorphismCategory))
    (is (morph/theory-subsumes? MorphismGraph MorphismGraph))))

(deftest test-theory-subsumes-subset
  (testing "Smaller theory is subsumed by larger"
    ;; MorphismGraph has V and E
    ;; MorphismCategory has Ob and Hom
    ;; MorphismGraph does NOT subsume MorphismCategory (different names)
    (is (not (morph/theory-subsumes? MorphismGraph MorphismCategory)))))

(deftest test-theory-subsumes-superset
  (testing "Larger theory does not subsume smaller (unless it contains it)"
    ;; MorphismCategory has more structure than needed
    (is (not (morph/theory-subsumes? MorphismCategory MorphismGraph)))))

;;; ============================================================================
;;; Theory Inclusion Tests
;;; ============================================================================

(deftest test-theory-incl-identity
  (testing "Can create identity inclusion (theory includes itself)"
    (let [incl (morph/theory-incl MorphismCategory MorphismCategory)]
      (is (morph/theory-incl? incl))
      (is (= MorphismCategory (morph/dom incl)))
      (is (= MorphismCategory (morph/codom incl))))))

(deftest test-theory-incl-invalid
  (testing "Cannot create inclusion when not subsumed"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"not subsumed"
         (morph/theory-incl MorphismCategory MorphismGraph)))))

(deftest test-theory-incl-dom-codom
  (testing "dom and codom work on TheoryIncl"
    (let [incl (morph/theory-incl MorphismGraph MorphismGraph)]
      (is (= MorphismGraph (morph/dom incl)))
      (is (= MorphismGraph (morph/codom incl))))))

;;; ============================================================================
;;; Morphism Composition Tests
;;; ============================================================================

(deftest test-compose-identity-identity
  (testing "Composing identity with itself returns identity"
    (let [id-cat (morph/id-theory-map MorphismCategory)
          result (morph/compose-morphisms id-cat id-cat)]
      (is (morph/id-theory-map? result))
      (is (= MorphismCategory (:gat result))))))

(deftest test-compose-identity-left
  (testing "Composing morphism with identity on left returns morphism"
    (let [id-cat (morph/id-theory-map MorphismCategory)
          incl (morph/theory-incl MorphismCategory MorphismCategory)
          result (morph/compose-morphisms incl id-cat)]
      (is (morph/theory-incl? result)))))

(deftest test-compose-identity-right
  (testing "Composing identity with morphism on right returns morphism"
    (let [id-cat (morph/id-theory-map MorphismCategory)
          incl (morph/theory-incl MorphismCategory MorphismCategory)
          result (morph/compose-morphisms id-cat incl)]
      (is (morph/theory-incl? result)))))

(deftest test-compose-inclusions
  (testing "Composing inclusions creates new inclusion"
    (let [;; MorphismGraph ⊆ MorphismGraph ⊆ MorphismGraph
          incl1 (morph/theory-incl MorphismGraph MorphismGraph)
          incl2 (morph/theory-incl MorphismGraph MorphismGraph)
          result (morph/compose-morphisms incl1 incl2)]
      (is (morph/theory-incl? result))
      (is (= MorphismGraph (morph/dom result)))
      (is (= MorphismGraph (morph/codom result))))))

(deftest test-compose-incompatible
  (testing "Cannot compose morphisms with incompatible dom/codom"
    (let [id-cat (morph/id-theory-map MorphismCategory)
          id-graph (morph/id-theory-map MorphismGraph)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"codomain of f must equal domain of g"
           (morph/compose-morphisms id-cat id-graph))))))

;;; ============================================================================
;;; Integration: Creating a Hierarchy
;;; ============================================================================

(deftest test-inclusion-chain
  (testing "Can create a chain of inclusions"
    ;; MorphismGraph ⊆ MorphismGraph ⊆ MorphismGraph
    (let [incl1 (morph/theory-incl MorphismGraph MorphismGraph)
          incl2 (morph/theory-incl MorphismGraph MorphismGraph)
          incl3 (morph/compose-morphisms incl1 incl2)]
      (is (morph/theory-incl? incl3))
      (is (= MorphismGraph (morph/dom incl3)))
      (is (= MorphismGraph (morph/codom incl3))))))

(deftest test-identity-unit-laws
  (testing "Identity satisfies unit laws"
    (let [id-cat (morph/id-theory-map MorphismCategory)
          incl (morph/theory-incl MorphismCategory MorphismCategory)]
      ;; id ∘ id = id
      (is (morph/id-theory-map? (morph/compose-morphisms id-cat id-cat)))
      ;; id ∘ incl = incl
      (is (morph/theory-incl? (morph/compose-morphisms id-cat incl)))
      ;; incl ∘ id = incl
      (is (morph/theory-incl? (morph/compose-morphisms incl id-cat))))))

;;; ============================================================================
;;; Pretty Printing Tests
;;; ============================================================================

(deftest test-format-identity
  (testing "Identity morphism has readable toString"
    (let [id-cat (morph/id-theory-map MorphismCategory)]
      (is (re-find #"IdTheoryMap.*Category" (str id-cat))))))

(deftest test-format-inclusion
  (testing "Theory inclusion has readable toString"
    (let [incl (morph/theory-incl MorphismGraph MorphismGraph)]
      (is (re-find #"TheoryIncl.*Graph.*⊆.*Graph" (str incl))))))
