(ns katzen.finset-test
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.finset :as fs]))

;; ============================================================================
;; FinSet
;; ============================================================================

(deftest test-finset-construction
  (is (fs/fin-set? (fs/fin-set 0)))
  (is (= 0 (fs/cardinality (fs/fin-set 0))))
  (is (= 5 (fs/cardinality (fs/fin-set 5))))
  (is (= '() (fs/elements (fs/fin-set 0))))
  (is (= [0 1 2 3 4] (vec (fs/elements (fs/fin-set 5)))))
  (is (thrown? Exception (fs/fin-set -1))))

;; ============================================================================
;; FinFunction
;; ============================================================================

(deftest test-finfunction-construction
  (let [f (fs/fin-function [1 2 0] 3)]
    (is (fs/fin-function? f))
    (is (= (fs/fin-set 3) (fs/dom f)))
    (is (= (fs/fin-set 3) (fs/cod f)))))

(deftest test-finfunction-app
  (let [f (fs/fin-function [2 0 1] 3)]
    (is (= 2 (fs/app f 0)))
    (is (= 0 (fs/app f 1)))
    (is (= 1 (fs/app f 2)))
    (is (thrown? Exception (fs/app f 3))
        "applying f to an out-of-range domain element throws")))

(deftest test-finfunction-rejects-out-of-range-image
  (is (thrown-with-msg? Exception #"out of codomain range"
                        (fs/fin-function [0 5 1] 3))))

;; ============================================================================
;; Identity + composition
;; ============================================================================

(deftest test-identity
  (let [id (fs/id-function 4)]
    (is (fs/identity-function? id))
    (is (= [0 1 2 3] (:vals id)))
    (is (= (fs/fin-set 4) (fs/cod id)))))

(deftest test-compose-identity-strips
  (let [f  (fs/fin-function [1 0 2] 3)
        id (fs/id-function 3)]
    (is (fs/fin-function= f (fs/compose id f)))
    (is (fs/fin-function= f (fs/compose f id)))))

(deftest test-compose-associativity
  (testing "(f ∘ g) ∘ h = f ∘ (g ∘ h)"
    (let [f (fs/fin-function [1 0 2] 3)
          g (fs/fin-function [2 1 0] 3)
          h (fs/fin-function [0 2 1] 3)
          left  (fs/compose (fs/compose f g) h)
          right (fs/compose f (fs/compose g h))]
      (is (fs/fin-function= left right)))))

(deftest test-compose-domain-mismatch
  (let [f (fs/fin-function [0 1] 2)
        g (fs/fin-function [0 1 0] 2)]   ; dom 3, but cod f is 2
    (is (thrown-with-msg? Exception #"codom\(f\) must equal dom\(g\)"
                          (fs/compose f g)))))

;; ============================================================================
;; Image / preimage / surjective / injective
;; ============================================================================

(deftest test-preimage
  (let [f (fs/fin-function [0 1 0 2 1 0] 3)]
    (is (= [0 2 5] (fs/preimage f 0)))
    (is (= [1 4]   (fs/preimage f 1)))
    (is (= [3]     (fs/preimage f 2)))))

(deftest test-image
  (let [f (fs/fin-function [0 1 0 0 1] 3)]
    (is (= #{0 1} (fs/image f)))))

(deftest test-surjective-injective-bijective
  (testing "A bijection on a 3-set"
    (let [f (fs/fin-function [2 0 1] 3)]
      (is (fs/surjective? f))
      (is (fs/injective? f))
      (is (fs/bijective? f))))
  (testing "Non-surjective map"
    (let [f (fs/fin-function [0 0 1] 3)]
      (is (not (fs/surjective? f)))
      (is (not (fs/injective? f)))
      (is (not (fs/bijective? f)))))
  (testing "Injection that's not a surjection"
    (let [f (fs/fin-function [0 1] 5)]
      (is (not (fs/surjective? f)))
      (is (fs/injective? f))
      (is (not (fs/bijective? f))))))

;; ============================================================================
;; Constant function
;; ============================================================================

(deftest test-constant-function
  (let [c (fs/constant-function 4 2 5)]
    (is (= [2 2 2 2] (:vals c)))
    (is (= (fs/fin-set 5) (fs/cod c)))))
