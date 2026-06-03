(ns katzen.presentation-test
  "Tests for GAT presentations."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.presentation :as pres]
            [katzen.theory :as theory]
            [katzen.core :as core]
            [katzen.scope :as scope]))

;;; ============================================================================
;;; Setup: Define theories for testing
;;; ============================================================================

;; PresentationGraph: Vertices and edges
(theory/deftheory PresentationGraph
  (type V)
  (type E [src V, tgt V]))

;; PresentationCategory: Objects and morphisms
(theory/deftheory PresentationCategory
  (type Ob)
  (type Hom [dom Ob, codom Ob])
  (term compose
    :ctx [a Ob, b Ob, c Ob]
    :args [f (Hom a b), g (Hom b c)]
    :ret (Hom a c))
  (term id
    :ctx [a Ob]
    :ret (Hom a a)))

;;; ============================================================================
;;; Presentation Creation Tests
;;; ============================================================================

(deftest test-empty-presentation
  (testing "Can create empty presentation"
    (let [p (pres/empty-presentation PresentationGraph)]
      (is (pres/presentation? p))
      (is (= PresentationGraph (:theory p)))
      (is (empty? (:generators p)))
      (is (empty? (:equations p))))))

(deftest test-presentation-constructor
  (testing "Can create presentation with constructor"
    (let [gens {'v1 {:name 'v1 :type 'V :index 0}}
          eqs [{:lhs 'x :rhs 'y}]
          p (pres/presentation PresentationGraph gens gens eqs)]
      (is (pres/presentation? p))
      (is (= 1 (count (:generators p))))
      (is (= 1 (count (:equations p)))))))

;;; ============================================================================
;;; Generator Management Tests
;;; ============================================================================

(deftest test-add-generator
  (testing "Can add a generator"
    (let [p (pres/empty-presentation PresentationGraph)
          p2 (pres/add-generator! p 'v1 'V)]
      (is (pres/has-generator? p2 'v1))
      (is (= 1 (count (:generators p2))))))

  (testing "Generator has correct type"
    (let [p (pres/empty-presentation PresentationGraph)
          p2 (pres/add-generator! p 'v1 'V)
          gen (pres/get-generator p2 'v1)]
      (is (= 'V (:type gen)))
      (is (= 'v1 (:name gen)))))

  (testing "Generator index increments"
    (let [p (pres/empty-presentation PresentationGraph)
          p2 (-> p
                 (pres/add-generator! 'v1 'V)
                 (pres/add-generator! 'v2 'V)
                 (pres/add-generator! 'e1 'E))]
      (is (= 0 (pres/generator-index p2 'v1)))
      (is (= 1 (pres/generator-index p2 'v2)))
      (is (= 0 (pres/generator-index p2 'e1))))))

(deftest test-add-generator-errors
  (testing "Cannot add duplicate generator"
    (let [p (pres/empty-presentation PresentationGraph)
          p2 (pres/add-generator! p 'v1 'V)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"already exists"
           (pres/add-generator! p2 'v1 'V)))))

  (testing "Cannot add generator with invalid type"
    (let [p (pres/empty-presentation PresentationGraph)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Type not found"
           (pres/add-generator! p 'x 'InvalidType))))))

(deftest test-add-generators-bulk
  (testing "Can add multiple generators at once"
    (let [p (pres/empty-presentation PresentationGraph)
          p2 (pres/add-generators! p [['v1 'V] ['v2 'V] ['e1 'E]])]
      (is (= 3 (count (:generators p2))))
      (is (pres/has-generator? p2 'v1))
      (is (pres/has-generator? p2 'v2))
      (is (pres/has-generator? p2 'e1)))))

(deftest test-has-generator
  (testing "has-generator? returns false for missing generator"
    (let [p (pres/empty-presentation PresentationGraph)]
      (is (not (pres/has-generator? p 'missing)))))

  (testing "has-generator? returns true for existing generator"
    (let [p (pres/empty-presentation PresentationGraph)
          p2 (pres/add-generator! p 'v1 'V)]
      (is (pres/has-generator? p2 'v1)))))

(deftest test-get-generator
  (testing "get-generator returns generator info"
    (let [p (pres/empty-presentation PresentationGraph)
          p2 (pres/add-generator! p 'v1 'V)
          gen (pres/get-generator p2 'v1)]
      (is (= 'v1 (:name gen)))
      (is (= 'V (:type gen)))
      (is (= 0 (:index gen)))))

  (testing "get-generator throws on missing generator"
    (let [p (pres/empty-presentation PresentationGraph)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"not found"
           (pres/get-generator p 'missing))))))

(deftest test-generators-query
  (testing "generators returns all generators"
    (let [p (-> (pres/empty-presentation PresentationGraph)
                (pres/add-generator! 'v1 'V)
                (pres/add-generator! 'e1 'E))
          all-gens (pres/generators p)]
      (is (= 2 (count all-gens)))))

  (testing "generators can filter by type"
    (let [p (-> (pres/empty-presentation PresentationGraph)
                (pres/add-generator! 'v1 'V)
                (pres/add-generator! 'v2 'V)
                (pres/add-generator! 'e1 'E))
          v-gens (pres/generators p 'V)
          e-gens (pres/generators p 'E)]
      (is (= 2 (count v-gens)))
      (is (= 1 (count e-gens))))))

;;; ============================================================================
;;; Equation Management Tests
;;; ============================================================================

(deftest test-add-equation
  (testing "Can add an equation"
    (let [p (pres/empty-presentation PresentationGraph)
          tag (scope/scope-tag)
          v-type (core/alg-type (scope/ident tag 0 'V) [] core/TYPE)
          lhs (core/alg-term (scope/ident tag 1 'x) [] v-type)
          rhs (core/alg-term (scope/ident tag 2 'y) [] v-type)
          p2 (pres/add-equation! p lhs rhs)]
      (is (= 1 (count (pres/equations p2))))
      (let [eq (first (pres/equations p2))]
        (is (= lhs (:lhs eq)))
        (is (= rhs (:rhs eq))))))

  (testing "Can add multiple equations"
    (let [p (pres/empty-presentation PresentationGraph)
          tag (scope/scope-tag)
          v-type (core/alg-type (scope/ident tag 0 'V) [] core/TYPE)
          lhs1 (core/alg-term (scope/ident tag 1 'x) [] v-type)
          rhs1 (core/alg-term (scope/ident tag 2 'y) [] v-type)
          lhs2 (core/alg-term (scope/ident tag 3 'a) [] v-type)
          rhs2 (core/alg-term (scope/ident tag 4 'b) [] v-type)
          p2 (-> p
                 (pres/add-equation! lhs1 rhs1)
                 (pres/add-equation! lhs2 rhs2))]
      (is (= 2 (count (pres/equations p2)))))))

(deftest test-add-equations-bulk
  (testing "Can add multiple equations at once"
    (let [p (pres/empty-presentation PresentationGraph)
          tag (scope/scope-tag)
          v-type (core/alg-type (scope/ident tag 0 'V) [] core/TYPE)
          lhs1 (core/alg-term (scope/ident tag 1 'x) [] v-type)
          rhs1 (core/alg-term (scope/ident tag 2 'y) [] v-type)
          lhs2 (core/alg-term (scope/ident tag 3 'a) [] v-type)
          rhs2 (core/alg-term (scope/ident tag 4 'b) [] v-type)
          p2 (pres/add-equations! p [[lhs1 rhs1] [lhs2 rhs2]])]
      (is (= 2 (count (pres/equations p2)))))))

(deftest test-equations-query
  (testing "equations returns empty vector for empty presentation"
    (let [p (pres/empty-presentation PresentationGraph)]
      (is (empty? (pres/equations p)))))

  (testing "equations returns all equations"
    (let [p (pres/empty-presentation PresentationGraph)
          tag (scope/scope-tag)
          v-type (core/alg-type (scope/ident tag 0 'V) [] core/TYPE)
          lhs (core/alg-term (scope/ident tag 1 'x) [] v-type)
          rhs (core/alg-term (scope/ident tag 2 'y) [] v-type)
          p2 (pres/add-equation! p lhs rhs)]
      (is (= 1 (count (pres/equations p2)))))))

;;; ============================================================================
;;; Definition Management Tests
;;; ============================================================================

(deftest test-add-definition
  (testing "add-definition adds both generator and equation"
    (let [p (pres/empty-presentation PresentationGraph)
          tag (scope/scope-tag)
          v-type (core/alg-type (scope/ident tag 0 'V) [] core/TYPE)
          rhs (core/alg-term (scope/ident tag 1 'base) [] v-type)
          p2 (pres/add-definition! p 'derived 'V rhs)]
      (is (pres/has-generator? p2 'derived))
      (is (= 1 (count (pres/equations p2))))))

  (testing "Definition equation has correct structure"
    (let [p (pres/empty-presentation PresentationGraph)
          tag (scope/scope-tag)
          v-type (core/alg-type (scope/ident tag 0 'V) [] core/TYPE)
          rhs (core/alg-term (scope/ident tag 1 'base) [] v-type)
          p2 (pres/add-definition! p 'derived 'V rhs)
          eq (first (pres/equations p2))]
      (is (= rhs (:rhs eq))))))

;;; ============================================================================
;;; Presentation Inheritance Tests
;;; ============================================================================

(deftest test-merge-presentations
  (testing "Can merge two presentations"
    (let [p1 (-> (pres/empty-presentation PresentationGraph)
                 (pres/add-generator! 'v1 'V))
          p2 (-> (pres/empty-presentation PresentationGraph)
                 (pres/add-generator! 'v2 'V))
          merged (pres/merge-presentations p1 p2)]
      (is (= 2 (count (:generators merged))))
      (is (pres/has-generator? merged 'v1))
      (is (pres/has-generator? merged 'v2))))

  (testing "Merge combines equations"
    (let [tag (scope/scope-tag)
          v-type (core/alg-type (scope/ident tag 0 'V) [] core/TYPE)
          lhs1 (core/alg-term (scope/ident tag 1 'x) [] v-type)
          rhs1 (core/alg-term (scope/ident tag 2 'y) [] v-type)
          lhs2 (core/alg-term (scope/ident tag 3 'a) [] v-type)
          rhs2 (core/alg-term (scope/ident tag 4 'b) [] v-type)
          p1 (-> (pres/empty-presentation PresentationGraph)
                 (pres/add-equation! lhs1 rhs1))
          p2 (-> (pres/empty-presentation PresentationGraph)
                 (pres/add-equation! lhs2 rhs2))
          merged (pres/merge-presentations p1 p2)]
      (is (= 2 (count (pres/equations merged)))))))

(deftest test-merge-presentation-errors
  (testing "Cannot merge presentations for different theories"
    (let [p1 (pres/empty-presentation PresentationGraph)
          p2 (pres/empty-presentation PresentationCategory)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"different theories"
           (pres/merge-presentations p1 p2)))))

  (testing "Cannot merge presentations with name conflicts"
    (let [p1 (-> (pres/empty-presentation PresentationGraph)
                 (pres/add-generator! 'v1 'V))
          p2 (-> (pres/empty-presentation PresentationGraph)
                 (pres/add-generator! 'v1 'V))]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"conflicts"
           (pres/merge-presentations p1 p2))))))

(deftest test-extend-presentation
  (testing "Can extend presentation with new generators and equations"
    (let [base (-> (pres/empty-presentation PresentationGraph)
                   (pres/add-generator! 'v1 'V))
          tag (scope/scope-tag)
          v-type (core/alg-type (scope/ident tag 0 'V) [] core/TYPE)
          lhs (core/alg-term (scope/ident tag 1 'x) [] v-type)
          rhs (core/alg-term (scope/ident tag 2 'y) [] v-type)
          extended (pres/extend-presentation base
                                              [['v2 'V] ['e1 'E]]
                                              [[lhs rhs]])]
      (is (= 3 (count (:generators extended))))
      (is (pres/has-generator? extended 'v1))
      (is (pres/has-generator? extended 'v2))
      (is (pres/has-generator? extended 'e1))
      (is (= 1 (count (pres/equations extended)))))))

;;; ============================================================================
;;; Pretty Printing Tests
;;; ============================================================================

(deftest test-format-presentation
  (testing "format-presentation produces readable output"
    (let [p (-> (pres/empty-presentation PresentationGraph)
                (pres/add-generator! 'v1 'V)
                (pres/add-generator! 'e1 'E))
          formatted (pres/format-presentation p)]
      (is (re-find #"Presentation.*Graph" formatted))
      (is (re-find #"Generators" formatted))
      (is (re-find #"v1" formatted))
      (is (re-find #"e1" formatted))))

  (testing "toString shows summary"
    (let [p (-> (pres/empty-presentation PresentationGraph)
                (pres/add-generator! 'v1 'V))]
      (is (re-find #"Presentation.*Graph.*1 generators" (str p))))))

;;; ============================================================================
;;; @present Macro Tests
;;; ============================================================================

(deftest test-defpresentation-macro
  (testing "Can define presentation with defpresentation macro"
    (pres/defpresentation TestGraphPres PresentationGraph
      (v1 :- V)
      (v2 :- V)
      (e1 :- E))
    (is (pres/presentation? TestGraphPres))
    (is (= 3 (count (:generators TestGraphPres))))
    (is (pres/has-generator? TestGraphPres 'v1))
    (is (pres/has-generator? TestGraphPres 'v2))
    (is (pres/has-generator? TestGraphPres 'e1))))

(deftest test-defpresentation-types
  (testing "Generators have correct types"
    (pres/defpresentation TestGraphPres2 PresentationGraph
      (a :- V)
      (b :- V)
      (f :- E))
    (let [gen-a (pres/get-generator TestGraphPres2 'a)
          gen-b (pres/get-generator TestGraphPres2 'b)
          gen-f (pres/get-generator TestGraphPres2 'f)]
      (is (= 'V (:type gen-a)))
      (is (= 'V (:type gen-b)))
      (is (= 'E (:type gen-f))))))

(deftest test-defpresentation-indices
  (testing "Generator indices are correct"
    (pres/defpresentation TestGraphPres3 PresentationGraph
      (x :- V)
      (y :- V)
      (z :- V))
    (is (= 0 (pres/generator-index TestGraphPres3 'x)))
    (is (= 1 (pres/generator-index TestGraphPres3 'y)))
    (is (= 2 (pres/generator-index TestGraphPres3 'z)))))

;;; ============================================================================
;;; Integration Tests
;;; ============================================================================

(deftest test-graph-presentation
  (testing "Can create a complete graph presentation"
    (let [p (-> (pres/empty-presentation PresentationGraph)
                (pres/add-generators! [['v1 'V]
                                       ['v2 'V]
                                       ['v3 'V]
                                       ['e1 'E]
                                       ['e2 'E]]))]
      (is (= 5 (count (:generators p))))
      (is (= 3 (count (pres/generators p 'V))))
      (is (= 2 (count (pres/generators p 'E)))))))

(deftest test-category-presentation
  (testing "Can create a category presentation"
    (let [p (-> (pres/empty-presentation PresentationCategory)
                (pres/add-generators! [['a 'Ob]
                                       ['b 'Ob]
                                       ['f 'Hom]
                                       ['g 'Hom]]))]
      (is (= 4 (count (:generators p))))
      (is (= 2 (count (pres/generators p 'Ob))))
      (is (= 2 (count (pres/generators p 'Hom)))))))
