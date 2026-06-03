(ns katzen.acset.check-test
  "Tests for runtime axiom checking of ACSet instances."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.acset :as a]
            [katzen.acset.check :as check]
            [katzen.acset.graphs :as gg]))

;; ============================================================================
;; Test schema: SchSymmetricGraph with involution + src/tgt-swap axioms
;; ============================================================================

(def SchSym
  "SchSymmetricGraph extended with the involution axiom."
  (assoc gg/SchSymmetricGraph
         :name 'SchSymmetricGraphAx
         :axioms [{:name 'inv-involution
                   :ctx [{:name 'e :type :E}]
                   :lhs '(inv (inv e))
                   :rhs 'e}]))

(def SchSymFull
  "Three axioms: involution + src/tgt-swap pair."
  (assoc gg/SchSymmetricGraph
         :name 'SchSymmetricGraphFull
         :axioms
         [{:name 'inv-involution :ctx [{:name 'e :type :E}]
           :lhs '(inv (inv e))   :rhs 'e}
          {:name 'src-of-inv     :ctx [{:name 'e :type :E}]
           :lhs '(src (inv e))   :rhs '(tgt e)}
          {:name 'tgt-of-inv     :ctx [{:name 'e :type :E}]
           :lhs '(tgt (inv e))   :rhs '(src e)}]))

(defn- good-sym-graph
  "Two vertices, one symmetric edge pair, all axioms satisfied."
  [schema]
  (let [g (a/vector-acset schema)
        [g _] (a/add-parts g :V 2)
        [g [e1 e2]] (a/add-parts g :E 2)]
    (-> g
        (a/set-subpart :src e1 1) (a/set-subpart :tgt e1 2)
        (a/set-subpart :src e2 2) (a/set-subpart :tgt e2 1)
        (a/set-subpart :inv e1 e2) (a/set-subpart :inv e2 e1))))

;; ============================================================================
;; Well-formed instances
;; ============================================================================

(deftest test-well-formed-instance-passes
  (testing "A correctly-built symmetric graph satisfies the involution axiom"
    (is (nil? (check/check-axioms (good-sym-graph SchSym))))))

(deftest test-empty-instance-vacuously-passes
  (testing "An axiom over an empty domain is vacuously satisfied"
    (is (nil? (check/check-axioms (a/vector-acset SchSym))))))

(deftest test-three-axiom-graph-passes
  (testing "All three axioms of SchSymmetricGraphFull check on the standard
            inv-paired graph"
    (is (nil? (check/check-axioms (good-sym-graph SchSymFull))))))

;; ============================================================================
;; Violations
;; ============================================================================

(deftest test-inv-pointing-wrong-fails-involution
  (testing "An edge whose inv-image doesn't reverse-pair fails the axiom"
    (let [g (good-sym-graph SchSym)
          [g [e3]] (a/add-parts g :E 1)
          g (-> g
                (a/set-subpart :src e3 1) (a/set-subpart :tgt e3 1)
                (a/set-subpart :inv e3 1))   ; inv e3 = e1, but inv e1 = e2 ≠ e3
          v (check/check-axioms g)]
      (is (some? v))
      (is (= 'inv-involution (:axiom v)))
      (is (= {'e 3} (:bindings v)))
      (is (= 2 (:lhs-eval v)))
      (is (= 3 (:rhs-eval v))))))

(deftest test-broken-src-swap-fails-second-axiom
  (testing "Breaking src(inv e) = tgt e is caught by the src-of-inv axiom"
    (let [;; Take a good 3-axiom graph and change src e2 from 2 to 1.
          g (a/set-subpart (good-sym-graph SchSymFull) :src 2 1)
          v (check/check-axioms g)]
      (is (some? v))
      (is (= 'src-of-inv (:axiom v))))))

;; ============================================================================
;; Partial-morphism semantics
;; ============================================================================

(deftest test-partial-morphism-is-vacuously-satisfied
  (testing "An edge with no :inv set passes — partial morphisms don't trigger
            spurious failures"
    (let [g (a/vector-acset SchSym)
          [g _] (a/add-parts g :V 1)
          [g [e1 e2]] (a/add-parts g :E 2)
          g (a/set-subpart g :inv e1 e2)]   ; inv e2 deliberately unset
      (is (nil? (check/check-axioms g))))))

;; ============================================================================
;; Strict variant
;; ============================================================================

(deftest test-check-axioms-bang-returns-on-pass-throws-on-fail
  (testing "check-axioms! is identity on a passing instance and throws
            on a failing one"
    (let [g (good-sym-graph SchSym)]
      (is (identical? g (check/check-axioms! g))))
    (let [bad (let [g (good-sym-graph SchSym)
                    [g [e3]] (a/add-parts g :E 1)]
                (a/set-subpart g :inv e3 1))]
      (is (thrown-with-msg? Exception #"ACSet axiom violation"
                            (check/check-axioms! bad))))))

;; ============================================================================
;; A non-axiomized schema is a no-op
;; ============================================================================

(deftest test-no-axioms-no-check
  (testing "A schema with no :axioms field passes trivially (no work done)"
    (let [g (let [[g _] (a/add-vertices (a/graph) 3)
                  [g _] (a/add-edge g 1 2)]
              g)]
      (is (nil? (check/check-axioms g))))))
