(ns katzen.finset.limits-test
  "Tests for FinSet limits.

   Each named limit has two tests: (1) the constructed apex + legs have the
   right shape, and (2) the universal arrow commutes — i.e. composing the
   universal with each leg recovers the input cone arrows. Commutation is
   the defining property; if it holds the limit is correct up to
   isomorphism."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.finset :as fs]
            [katzen.finset.limits :as lim]))

;; ============================================================================
;; Terminal
;; ============================================================================

(deftest test-terminal
  (let [t (lim/terminal)]
    (is (= 1 (fs/cardinality (:apex t))))
    (is (empty? (:legs t)))
    (testing "universal arrow is the constant 0"
      (let [u (lim/universal t (fs/fin-set 5))]
        (is (= [0 0 0 0 0] (:vals u)))
        (is (= 1 (fs/cardinality (fs/cod u))))))))

;; ============================================================================
;; Product (binary)
;; ============================================================================

(deftest test-product-shape
  (testing "|A × B| = |A| · |B|; projections compute column-major decode"
    (let [p (lim/product (fs/fin-set 2) (fs/fin-set 3))]
      (is (= 6 (fs/cardinality (:apex p))))
      (is (= [0 1 0 1 0 1] (:vals (first (:legs p)))))
      (is (= [0 0 1 1 2 2] (:vals (second (:legs p))))))))

(deftest test-product-universal-commutes
  (testing "pair(f, g) followed by each projection recovers f and g"
    (let [p (lim/product (fs/fin-set 2) (fs/fin-set 3))
          f (fs/fin-function [1 0 1] 2)
          g (fs/fin-function [2 0 1] 3)
          u (lim/universal p [f g])
          [pi1 pi2] (:legs p)]
      (is (fs/fin-function= f (fs/compose u pi1)))
      (is (fs/fin-function= g (fs/compose u pi2))))))

(deftest test-product-n-empty-is-terminal
  (let [p (lim/product-n [])]
    (is (= 1 (fs/cardinality (:apex p))))))

(deftest test-product-n-three-factors
  (testing "Triple product 2×3×2 has 12 elements; projections recover the
            corresponding mixed-radix digit. Strides are [1 2 6]."
    (let [p (lim/product-n [(fs/fin-set 2) (fs/fin-set 3) (fs/fin-set 2)])
          [p0 p1 p2] (:legs p)
          ;; k = 9 = 1·1 + 1·2 + 1·6 → (1, 1, 1).
          k 9]
      (is (= 12 (fs/cardinality (:apex p))))
      (is (= 1 (fs/app p0 k)))
      (is (= 1 (fs/app p1 k)))
      (is (= 1 (fs/app p2 k))))))

;; ============================================================================
;; Equalizer
;; ============================================================================

(deftest test-equalizer-shape
  (testing "Equalizer of f, g picks out positions where f=g"
    (let [f (fs/fin-function [0 1 2 1 0] 3)
          g (fs/fin-function [0 0 2 1 1] 3)
          eq (lim/equalizer f g)]
      (is (= 3 (fs/cardinality (:apex eq))))
      (is (= [0 2 3] (:vals (first (:legs eq))))))))

(deftest test-equalizer-universal-commutes
  (testing "Any h that equalizes f and g factors through the equalizer
            inclusion"
    (let [f  (fs/fin-function [0 1 2 1 0] 3)
          g  (fs/fin-function [0 0 2 1 1] 3)
          eq (lim/equalizer f g)
          h  (fs/fin-function [0 3] 5)   ; 0 and 3 both equalize
          u  (lim/universal eq h)
          incl (first (:legs eq))]
      (is (fs/fin-function= h (fs/compose u incl))))))

(deftest test-equalizer-universal-rejects-non-equalizer
  (testing "Universal property fails fast if h doesn't equalize"
    (let [f  (fs/fin-function [0 1 2] 3)
          g  (fs/fin-function [1 0 2] 3)
          eq (lim/equalizer f g)
          h  (fs/fin-function [0] 3)]    ; f(0)=0, g(0)=1 — does NOT equalize
      (is (thrown-with-msg? Exception #"does not equalize"
                            (lim/universal eq h))))))

;; ============================================================================
;; Pullback
;; ============================================================================

(deftest test-pullback-shape
  (testing "Pullback enumerates {(a,b) | f(a)=g(b)}"
    (let [f (fs/fin-function [0 0 1] 2)
          g (fs/fin-function [0 1 1] 2)
          pb (lim/pullback f g)]
      (is (= 4 (fs/cardinality (:apex pb))))
      (is (= [[0 0] [1 0] [2 1] [2 2]] (:pairs pb))))))

(deftest test-pullback-universal-commutes
  (testing "Any commuting cone (h, k) over the cospan factors through the
            pullback projections"
    (let [f (fs/fin-function [0 0 1] 2)
          g (fs/fin-function [0 1 1] 2)
          pb (lim/pullback f g)
          h  (fs/fin-function [0 2] 3)
          k  (fs/fin-function [0 1] 3)
          u  (lim/universal pb [h k])
          [pi1 pi2] (:legs pb)]
      (is (fs/fin-function= h (fs/compose u pi1)))
      (is (fs/fin-function= k (fs/compose u pi2))))))

(deftest test-pullback-universal-rejects-non-commuting
  (testing "Universal property fails if the cone doesn't commute"
    (let [f  (fs/fin-function [0 0 1] 2)
          g  (fs/fin-function [0 1 1] 2)
          pb (lim/pullback f g)
          h  (fs/fin-function [0] 3)
          k  (fs/fin-function [1] 3)]    ; f(0)=0, g(1)=1, square does NOT commute
      (is (thrown-with-msg? Exception #"does not commute"
                            (lim/universal pb [h k]))))))
