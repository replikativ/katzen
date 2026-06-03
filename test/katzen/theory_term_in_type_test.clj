(ns katzen.theory-term-in-type-test
  "Tests for term applications in type arguments.

  This tests the parser extension that allows term applications like
  (otimes a b) to appear as arguments to type constructors like Hom."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.theory :as theory]
            [katzen.core :as core]))

;;; ============================================================================
;;; Simple Monoidal Category Tests
;;; ============================================================================

(theory/deftheory SimpleMonoidalCategory
  (type Ob)
  (type Hom [dom Ob, codom Ob])

  (term otimes
    :ctx [a Ob, b Ob]
    :ret Ob)

  (term braid
    :ctx [a Ob, b Ob]
    :ret (Hom (otimes a b) (otimes b a))))

(deftest test-simple-monoidal-category
  (testing "Can define theory with term applications in type arguments"
    (is (core/gat? SimpleMonoidalCategory))
    (is (= 'SimpleMonoidalCategory (:name SimpleMonoidalCategory)))

    ;; Should have 2 types
    (is (= 2 (count (:type-constructors SimpleMonoidalCategory))))

    ;; Should have 2 terms: otimes and braid
    (is (= 2 (count (:term-constructors SimpleMonoidalCategory))))))

(deftest test-braid-term-structure
  (testing "Braid term has correct structure with nested term applications"
    (let [braid-term (second (:term-constructors SimpleMonoidalCategory))
          return-type (:type (:term braid-term))]

      ;; Return type should be Hom
      (is (= 'Hom (-> return-type :head :name)))

      ;; Should have 2 arguments (domain and codomain)
      (is (= 2 (count (:args return-type))))

      ;; First argument should be (otimes a b)
      (let [dom (first (:args return-type))]
        (is (core/alg-term? dom))
        (is (= 'otimes (-> dom :head :name)))
        (is (= 2 (count (:args dom))))
        (is (= 'a (-> dom :args first :name)))
        (is (= 'b (-> dom :args second :name))))

      ;; Second argument should be (otimes b a)
      (let [codom (second (:args return-type))]
        (is (core/alg-term? codom))
        (is (= 'otimes (-> codom :head :name)))
        (is (= 2 (count (:args codom))))
        (is (= 'b (-> codom :args first :name)))
        (is (= 'a (-> codom :args second :name)))))))

;;; ============================================================================
;;; Nested Term Application Tests
;;; ============================================================================

(theory/deftheory NestedTermCategory
  (type Ob)
  (type Hom [dom Ob, codom Ob])

  (term otimes
    :ctx [a Ob, b Ob]
    :ret Ob)

  (term associator
    :ctx [a Ob, b Ob, c Ob]
    :ret (Hom (otimes (otimes a b) c) (otimes a (otimes b c)))))

(deftest test-nested-term-applications
  (testing "Can handle nested term applications like (otimes (otimes a b) c)"
    (is (core/gat? NestedTermCategory))

    (let [assoc-term (second (:term-constructors NestedTermCategory))
          return-type (:type (:term assoc-term))]

      ;; Return type is Hom
      (is (= 'Hom (-> return-type :head :name)))

      ;; Domain: (otimes (otimes a b) c)
      (let [dom (first (:args return-type))]
        (is (core/alg-term? dom))
        (is (= 'otimes (-> dom :head :name)))

        ;; First arg of dom is (otimes a b)
        (let [nested (first (:args dom))]
          (is (core/alg-term? nested))
          (is (= 'otimes (-> nested :head :name)))
          (is (= 'a (-> nested :args first :name)))
          (is (= 'b (-> nested :args second :name))))

        ;; Second arg of dom is c
        (is (= 'c (-> dom :args second :name))))

      ;; Codomain: (otimes a (otimes b c))
      (let [codom (second (:args return-type))]
        (is (core/alg-term? codom))
        (is (= 'otimes (-> codom :head :name)))

        ;; First arg of codom is a
        (is (= 'a (-> codom :args first :name)))

        ;; Second arg of codom is (otimes b c)
        (let [nested (second (:args codom))]
          (is (core/alg-term? nested))
          (is (= 'otimes (-> nested :head :name)))
          (is (= 'b (-> nested :args first :name)))
          (is (= 'c (-> nested :args second :name))))))))

;;; ============================================================================
;;; Multiple Term Constructors Tests
;;; ============================================================================

(theory/deftheory RichMonoidalCategory
  (type Ob)
  (type Hom [dom Ob, codom Ob])

  (term otimes
    :ctx [a Ob, b Ob]
    :ret Ob)

  (term munit
    :ret Ob)

  (term left-unitor
    :ctx [a Ob]
    :ret (Hom (otimes munit a) a))

  (term right-unitor
    :ctx [a Ob]
    :ret (Hom (otimes a munit) a)))

(deftest test-multiple-term-constructors
  (testing "Can use multiple different term constructors in type arguments"
    (is (core/gat? RichMonoidalCategory))

    (let [left-unit (nth (:term-constructors RichMonoidalCategory) 2)
          right-unit (nth (:term-constructors RichMonoidalCategory) 3)]

      ;; Left unitor: (Hom (otimes munit a) a)
      (let [return-type (:type (:term left-unit))
            dom (first (:args return-type))]
        (is (core/alg-term? dom))
        (is (= 'otimes (-> dom :head :name)))
        (is (= 'munit (-> dom :args first :head :name))) ; munit is nullary term
        (is (= 'a (-> dom :args second :name))))

      ;; Right unitor: (Hom (otimes a munit) a)
      (let [return-type (:type (:term right-unit))
            dom (first (:args return-type))]
        (is (core/alg-term? dom))
        (is (= 'otimes (-> dom :head :name)))
        (is (= 'a (-> dom :args first :name)))
        (is (= 'munit (-> dom :args second :head :name))))))) ; munit is nullary term

;;; ============================================================================
;;; Backwards Compatibility Tests
;;; ============================================================================

(theory/deftheory TermInTypeSimpleCategory
  (type Ob)
  (type Hom [dom Ob, codom Ob])

  (term compose
    :ctx [a Ob, b Ob, c Ob]
    :args [f (Hom a b), g (Hom b c)]
    :ret (Hom a c))

  (term id
    :ctx [a Ob]
    :ret (Hom a a)))

(deftest test-backwards-compatibility
  (testing "Old-style type arguments (just symbols) still work"
    (is (core/gat? TermInTypeSimpleCategory))

    (let [compose-term (first (:term-constructors TermInTypeSimpleCategory))
          return-type (:type (:term compose-term))]

      ;; Return type is (Hom a c) with simple symbol arguments
      (is (= 'Hom (-> return-type :head :name)))
      (is (= 2 (count (:args return-type))))

      ;; Arguments are simple idents, not AlgTerms
      (let [dom (first (:args return-type))
            codom (second (:args return-type))]
        (is (not (core/alg-term? dom)))
        (is (not (core/alg-term? codom)))
        (is (= 'a (:name dom)))
        (is (= 'c (:name codom)))))))

;;; ============================================================================
;;; Error Handling Tests
;;; ============================================================================

(defn- root-cause-message
  "Walk the cause chain to the innermost exception and return its message.
   Clojure's Compiler wraps macroexpand-time exceptions in a
   CompilerException, so the original ex-info from the parser is the
   cause (or cause-of-cause) of what the test catches."
  [^Throwable t]
  (loop [cur t]
    (if-let [c (.getCause cur)]
      (recur c)
      (.getMessage cur))))

(deftest test-unbound-term-constructor
  (testing "Parser rejects unbound term constructors in type arguments.
            The parser fires its check at macro-expansion time, so the
            ex-info we throw is wrapped by Compiler$CompilerException;
            we walk the cause chain to find the original message."
    (let [thrown (try
                   (eval '(katzen.theory/deftheory BadTheory
                            (type Ob)
                            (type Hom [dom Ob, codom Ob])

                            (term foo
                              :ctx [a Ob]
                              :ret (Hom (undefined-term a) a))))
                   nil
                   (catch Throwable t t))]
      (is (some? thrown) "the bad theory must throw")
      (is (re-find #"Unbound term constructor" (root-cause-message thrown))))))

(deftest test-unbound-variable-in-term
  (testing "Parser rejects unbound variables in nested terms (same wrapping)"
    (let [thrown (try
                   (eval '(katzen.theory/deftheory BadTheory2
                            (type Ob)
                            (type Hom [dom Ob, codom Ob])

                            (term otimes
                              :ctx [a Ob, b Ob]
                              :ret Ob)

                            (term bad
                              :ctx [a Ob]
                              :ret (Hom (otimes a undefined-var) a))))
                   nil
                   (catch Throwable t t))]
      (is (some? thrown))
      (is (re-find #"Unbound" (root-cause-message thrown))))))
