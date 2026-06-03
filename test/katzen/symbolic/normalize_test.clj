(ns katzen.symbolic.normalize-test
  "Tests for AlgTerm-level normalization. Uses ThMonoid (3 axioms:
   assoc non-orientable; unit-left, unit-right size-decreasing) and
   ThGroup (adds inv-left, inv-right) as concrete theories.

   The representational subtlety to keep in mind: in katzen AlgTerm,
   a context-variable reference appears either as a bare Ident (when
   in arg position) or as an AlgTerm-leaf with empty :args (when as
   the whole top-level term). `term-equal?` smooths that over."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.core :as gc]
            [katzen.stdlib.core :as std]
            [katzen.symbolic.normalize :as sn]))

;; ============================================================================
;; Orientation
;; ============================================================================

(deftest test-thmonoid-orientation-counts
  (testing "ThMonoid has 3 axioms; 2 are orientable, assoc is not"
    (let [r (sn/axiom-orientation-report std/ThMonoid)]
      (is (= 3 (:total r)))
      (is (= 2 (:orientable r)))
      (is (= ['assoc] (:non-orientable r))))))

(deftest test-thgroup-orientation-counts
  (testing "ThGroup adds inv-left, inv-right — both orientable"
    (let [r (sn/axiom-orientation-report std/ThGroup)]
      (is (= 5 (:total r)))
      (is (= 4 (:orientable r)))
      (is (= ['assoc] (:non-orientable r))))))

;; ============================================================================
;; Helpers to construct AlgTerms using the actual theory idents
;; ============================================================================

(defn- find-axiom [gat name]
  (some #(when (= name (:name %)) %) (:axioms gat)))

(def unit-left-ax (find-axiom std/ThMonoid 'unit-left))
(def unit-right-ax (find-axiom std/ThMonoid 'unit-right))

(def x-bare (-> unit-left-ax :lhs :args second))   ; bare Ident
(def unit-leaf (-> unit-left-ax :lhs :args first)) ; AlgTerm (unit)
(def mul-head (-> unit-left-ax :lhs :head))
(def el-type  (-> unit-leaf :type))

(defn- mul [a b]
  (gc/alg-term mul-head [a b] el-type))

;; ============================================================================
;; Single-axiom normalization
;; ============================================================================

(deftest test-unit-left-rewrites
  (testing "(mul (unit) x) normalizes to x"
    (is (sn/equiv? std/ThMonoid (:lhs unit-left-ax) (:rhs unit-left-ax)))))

(deftest test-unit-right-rewrites
  (testing "(mul x (unit)) normalizes to x"
    (is (sn/equiv? std/ThMonoid (:lhs unit-right-ax) (:rhs unit-right-ax)))))

;; ============================================================================
;; Multi-rewrite cascades
;; ============================================================================

(deftest test-nested-unit-cancellations-cascade
  (testing "(mul (unit) (mul x (unit))) normalizes to x"
    (let [t (mul unit-leaf (mul x-bare unit-leaf))
          n (sn/normalize std/ThMonoid t)]
      (is (sn/term-equal? n x-bare)))))

(deftest test-deep-unit-cancellations
  (testing "Triple-nested unit on both sides reduces to x in one fixed-point loop"
    (let [t (mul unit-leaf (mul unit-leaf (mul x-bare (mul unit-leaf unit-leaf))))
          n (sn/normalize std/ThMonoid t)]
      (is (sn/term-equal? n x-bare)))))

(deftest test-non-orientable-axiom-no-rewrite
  (testing "An associativity-shaped target is left alone (assoc is non-orientable)"
    (let [t (mul (mul x-bare x-bare) x-bare)
          n (sn/normalize std/ThMonoid t)]
      (is (sn/term-equal? n t)
          "no rule applies; same structure preserved"))))

;; ============================================================================
;; term-equal? handles shape variation
;; ============================================================================

(deftest test-term-equal-handles-leaf-vs-bare-ident
  (testing "AlgTerm-wrapped x and bare-Ident x are equal under term-equal?"
    (let [wrapped (gc/alg-term x-bare [] el-type)]
      (is (sn/term-equal? wrapped x-bare))
      (is (sn/term-equal? x-bare wrapped))
      (is (sn/term-equal? wrapped wrapped))
      (is (sn/term-equal? x-bare x-bare)))))

;; ============================================================================
;; Canonical-form hints for associativity
;; ============================================================================
;;
;; ThMonoid's assoc axiom is non-orientable by size — both sides have
;; node count 4. With `:canonical :lhs` (= left-associate) it becomes a
;; directed rule.

(def ThMonoid-leftassoc
  (assoc std/ThMonoid :axioms
         (mapv (fn [ax] (if (= 'assoc (:name ax))
                         (assoc ax :canonical :lhs)
                         ax))
               (:axioms std/ThMonoid))))

(def assoc-ax (find-axiom std/ThMonoid 'assoc))

;; assoc axiom: (= (mul (mul x y) z) (mul x (mul y z))).
;; Pull bare idents from its LHS so the constructed test terms share scope.
(def x-id (-> assoc-ax :lhs :args first :args first))
(def y-id (-> assoc-ax :lhs :args first :args second))
(def z-id (-> assoc-ax :lhs :args second))

(deftest test-canonical-hint-makes-assoc-orientable
  (testing "ThMonoid with `:canonical :lhs` on assoc has all 3 axioms oriented"
    (let [r (sn/axiom-orientation-report ThMonoid-leftassoc)]
      (is (= 3 (:orientable r)))
      (is (empty? (:non-orientable r))))))

(deftest test-right-assoc-rewrites-to-left
  (testing "(mul x (mul y z)) normalizes to (mul (mul x y) z)"
    (let [right-assoc (mul x-id (mul y-id z-id))
          left-assoc  (mul (mul x-id y-id) z-id)
          norm (sn/normalize ThMonoid-leftassoc right-assoc)]
      (is (sn/term-equal? norm left-assoc)))))

(deftest test-already-left-assoc-is-fixed-point
  (let [t (mul (mul x-id y-id) z-id)]
    (is (sn/term-equal? t (sn/normalize ThMonoid-leftassoc t)))))
