(ns katzen.finset.colimits-test
  "Tests for FinSet colimits — initial, coproduct, coequalizer, pushout.

   As in the limits tests, each colimit is verified by (1) shape of the
   apex and legs, then (2) commutation of the universal arrow with the
   constructed legs."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.finset :as fs]
            [katzen.finset.colimits :as colim]))

;; ============================================================================
;; Initial
;; ============================================================================

(deftest test-initial
  (let [i (colim/initial)]
    (is (= 0 (fs/cardinality (:apex i))))
    (testing "universal arrow out of the empty set has no values"
      (let [u (colim/universal i (fs/fin-set 7))]
        (is (= [] (:vals u)))
        (is (= 7 (fs/cardinality (fs/cod u))))))))

;; ============================================================================
;; Coproduct
;; ============================================================================

(deftest test-coproduct-shape
  (testing "|A + B| = |A| + |B|; inclusions place A and B in disjoint segments"
    (let [c (colim/coproduct (fs/fin-set 2) (fs/fin-set 3))]
      (is (= 5 (fs/cardinality (:apex c))))
      (is (= [0 1]     (:vals (first (:legs c)))))
      (is (= [2 3 4]   (:vals (second (:legs c))))))))

(deftest test-coproduct-universal-commutes
  (testing "copair(f, g) post-composed with each inclusion recovers f and g"
    (let [c (colim/coproduct (fs/fin-set 2) (fs/fin-set 3))
          f (fs/fin-function [3 2]   4)
          g (fs/fin-function [0 1 3] 4)
          u (colim/universal c [f g])
          [i1 i2] (:legs c)]
      (is (fs/fin-function= f (fs/compose i1 u)))
      (is (fs/fin-function= g (fs/compose i2 u))))))

(deftest test-coproduct-n-empty-is-initial
  (let [c (colim/coproduct-n [])]
    (is (= 0 (fs/cardinality (:apex c))))))

(deftest test-coproduct-n-three-summands
  (testing "Triple coproduct 2+3+2 = 7; inclusions place each summand at the
            right offset"
    (let [c (colim/coproduct-n [(fs/fin-set 2) (fs/fin-set 3) (fs/fin-set 2)])
          [i0 i1 i2] (:legs c)]
      (is (= 7 (fs/cardinality (:apex c))))
      (is (= [0 1]     (:vals i0)))
      (is (= [2 3 4]   (:vals i1)))
      (is (= [5 6]     (:vals i2))))))

;; ============================================================================
;; Coequalizer
;; ============================================================================

(deftest test-coequalizer-shape
  (testing "Coequalizer of f, g : A → B merges f(i), g(i) for every i"
    (let [;; equates 0~1 and 2~3 → classes {0,1}, {2,3}, {4} → apex size 3
          f  (fs/fin-function [0 2] 5)
          g  (fs/fin-function [1 3] 5)
          ce (colim/coequalizer f g)]
      (is (= 3 (fs/cardinality (:apex ce))))
      (is (= [0 0 1 1 2] (:vals (first (:legs ce))))))))

(deftest test-coequalizer-universal-commutes
  (testing "Any h that coequalizes f, g factors uniquely through the projection"
    (let [f  (fs/fin-function [0 2] 5)
          g  (fs/fin-function [1 3] 5)
          ce (colim/coequalizer f g)
          h  (fs/fin-function [3 3 0 0 1] 4)
          u  (colim/universal ce h)
          proj (first (:legs ce))]
      (is (fs/fin-function= h (fs/compose proj u))))))

(deftest test-coequalizer-universal-rejects-non-coequalizer
  (testing "Universal property fails if h doesn't coequalize"
    (let [f  (fs/fin-function [0 1] 3)
          g  (fs/fin-function [1 2] 3)
          ce (colim/coequalizer f g)
          ;; h(0) ≠ h(1) — doesn't coequalize the first pair
          h  (fs/fin-function [3 5 5] 7)]
      (is (thrown-with-msg? Exception #"does not coequalize"
                            (colim/universal ce h))))))

(deftest test-coequalizer-of-equal-arrows-is-identity
  (testing "When f = g the coequalizer is the identity on B"
    (let [f (fs/fin-function [0 1 2] 4)
          ce (colim/coequalizer f f)
          proj (first (:legs ce))]
      (is (= 4 (fs/cardinality (:apex ce))))
      (is (fs/identity-function? proj)))))

;; ============================================================================
;; Pushout
;; ============================================================================

(deftest test-pushout-shape
  (testing "Pushout of span identifies images of f and g pointwise"
    (let [f  (fs/fin-function [0 1] 3)
          g  (fs/fin-function [0 0] 2)
          po (colim/pushout f g)]
      ;; A+B = 5; equate (incl_A 0, incl_B 0) = (0,3) and (incl_A 1, incl_B 0) = (1,3).
      ;; Classes: {0,1,3}, {2}, {4}.
      (is (= 3 (fs/cardinality (:apex po)))))))

(deftest test-pushout-universal-commutes
  (testing "A commuting cocone (y, z) factors through the pushout"
    (let [f  (fs/fin-function [0 1] 3)
          g  (fs/fin-function [0 0] 2)
          po (colim/pushout f g)
          ;; y(0)=y(1)=z(0)=1, z(1) and y(2) free
          y  (fs/fin-function [1 1 2] 3)
          z  (fs/fin-function [1 0] 3)
          u  (colim/universal po [y z])
          [leg-a leg-b] (:legs po)]
      (is (fs/fin-function= y (fs/compose leg-a u)))
      (is (fs/fin-function= z (fs/compose leg-b u))))))

(deftest test-pushout-of-span-with-empty-apex
  (testing "Pushout of an empty span A ← 0 → B is the coproduct A + B"
    (let [empty-fn-to-a (fs/fin-function [] 3)
          empty-fn-to-b (fs/fin-function [] 2)
          po (colim/pushout empty-fn-to-a empty-fn-to-b)
          coprod (colim/coproduct (fs/fin-set 3) (fs/fin-set 2))]
      (is (= (fs/cardinality (:apex coprod))
             (fs/cardinality (:apex po)))))))
