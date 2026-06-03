(ns katzen.ansatz.export-test
  "Tests for katzen.ansatz.export. Lives under test-ansatz/ and is only
   run via the :test-ansatz alias, since it requires a pre-built ansatz
   Mathlib store at /var/tmp/ansatz-mathlib."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [ansatz.core :as a]
            [katzen.ansatz.export :as ax]
            [katzen.stdlib.core :as std]
            [katzen.test-support :as ts]))

(defn- ansatz-ready? [] (ts/ansatz-ready?))

(use-fixtures :once ts/ensure-ansatz-init!)

;;; ============================================================================
;;; check-theory! over the standard library
;;; ============================================================================

(deftest test-check-theory-thgraph
  (when (ansatz-ready?)
    (testing "ThGraph passes the CIC kernel check"
      (is (= :ok (ax/check-theory! std/ThGraph))))))

(deftest test-check-theory-thmonoid
  (when (ansatz-ready?)
    (testing "ThMonoid + its three axioms (assoc, unit-left, unit-right) verify"
      (is (= :ok (ax/check-theory! std/ThMonoid))))))

(deftest test-check-theory-thcategory
  (when (ansatz-ready?)
    (testing "ThCategory with dependent Hom and the three category axioms verify"
      (is (= :ok (ax/check-theory! std/ThCategory))))))

(deftest test-check-theory-thgroup
  (when (ansatz-ready?)
    (testing "ThGroup verifies — inheritance via `using` reaches the kernel cleanly"
      (is (= :ok (ax/check-theory! std/ThGroup))))))

(deftest test-check-theory-thsymmoncat
  (when (ansatz-ready?)
    (testing "ThSymmetricMonoidalCategory verifies — two levels of inheritance"
      (is (= :ok (ax/check-theory! std/ThSymmetricMonoidalCategory))))))

(deftest test-check-theory-non-destructive
  (when (ansatz-ready?)
    (testing "check-theory! leaves the global env unchanged"
      (let [size-before (.size (a/env))]
        (ax/check-theory! std/ThMonoid)
        (is (= size-before (.size (a/env))))))))

;;; ============================================================================
;;; check-instance! shape checking
;;; ============================================================================

(deftest test-check-instance-monoid-nat
  (when (ansatz-ready?)
    (testing "Nat is a valid carrier for ThMonoid"
      (is (= :ok (ax/check-instance! std/ThMonoid '{El Nat}))))))

(deftest test-check-instance-monoid-bool
  (when (ansatz-ready?)
    (testing "Bool is a valid carrier for ThMonoid"
      (is (= :ok (ax/check-instance! std/ThMonoid '{El Bool}))))))

(deftest test-check-instance-category-discrete
  (when (ansatz-ready?)
    (testing "Discrete category on Nat — Hom = λ _ _ ⇒ Nat — type-checks"
      (is (= :ok (ax/check-instance! std/ThCategory
                                     '{Ob Nat
                                       Hom (lam [a Nat, b Nat] Nat)}))))))

(deftest test-check-instance-missing-binding
  (when (ansatz-ready?)
    (testing "Missing a sort binding throws with a clear message"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Missing sort bindings"
           (ax/check-instance! std/ThCategory '{Ob Nat}))))))

(deftest test-check-instance-rejects-non-gat
  (when (ansatz-ready?)
    (testing "check-instance! rejects non-GAT inputs"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"expects a katzen GAT"
           (ax/check-instance! :not-a-gat '{El Nat}))))))
