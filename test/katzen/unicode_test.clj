(ns katzen.unicode-test
  "Tests for unicode operator support in theory definitions."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.theory :as theory]
            [katzen.core :as core]
            [katzen.unicode :as unicode]))

;;; ============================================================================
;;; Unicode Alias Tests
;;; ============================================================================

(deftest test-normalize-name
  (testing "Unicode symbols normalize to ASCII"
    (is (= 'compose (unicode/normalize-name '⋅)))
    (is (= 'Hom (unicode/normalize-name '→)))
    (is (= 'otimes (unicode/normalize-name '⊗)))
    (is (= 'munit (unicode/normalize-name 'I)))
    (is (= 'associator (unicode/normalize-name 'α)))
    (is (= 'left-unitor (unicode/normalize-name 'λ)))
    (is (= 'right-unitor (unicode/normalize-name 'ρ)))
    (is (= 'braid (unicode/normalize-name 'σ))))

  (testing "ASCII symbols remain unchanged"
    (is (= 'compose (unicode/normalize-name 'compose)))
    (is (= 'Hom (unicode/normalize-name 'Hom)))
    (is (= 'otimes (unicode/normalize-name 'otimes))))

  (testing "Unknown symbols remain unchanged"
    (is (= 'unknown (unicode/normalize-name 'unknown)))))

(deftest test-unicode-name
  (testing "ASCII symbols get unicode forms"
    (is (= '⋅ (unicode/unicode-name 'compose)))
    (is (= '→ (unicode/unicode-name 'Hom)))
    (is (= '⊗ (unicode/unicode-name 'otimes)))
    (is (= 'I (unicode/unicode-name 'munit))))

  (testing "Unicode symbols remain unchanged"
    (is (= '⋅ (unicode/unicode-name '⋅)))
    (is (= '→ (unicode/unicode-name '→))))

  (testing "Unknown symbols remain unchanged"
    (is (= 'unknown (unicode/unicode-name 'unknown)))))

;;; ============================================================================
;;; Parser Tests - Unicode Type Names
;;; ============================================================================

(theory/deftheory UnicodeCategory
  (type Ob)
  (type → [dom Ob, codom Ob])  ; Unicode arrow instead of Hom

  (term ⋅                      ; Unicode compose
    :ctx [a Ob, b Ob, c Ob]
    :args [f (→ a b), g (→ b c)]
    :ret (→ a c))

  (term id
    :ctx [a Ob]
    :ret (→ a a)))

(deftest test-unicode-theory-definition
  (testing "Theory with unicode operators is valid"
    (is (core/gat? UnicodeCategory))
    (is (= 'UnicodeCategory (:name UnicodeCategory))))

  (testing "Types are normalized to ASCII internally"
    (is (= 2 (count (:type-constructors UnicodeCategory))))
    (let [type-names (map #(-> % :type :head :name) (:type-constructors UnicodeCategory))]
      (is (some #(= 'Ob %) type-names))
      ;; → should be normalized to Hom
      (is (some #(= 'Hom %) type-names))))

  (testing "Terms are normalized to ASCII internally"
    (is (= 2 (count (:term-constructors UnicodeCategory))))
    (let [term-names (map #(-> % :term :head :name) (:term-constructors UnicodeCategory))]
      ;; ⋅ should be normalized to compose
      (is (some #(= 'compose %) term-names))
      (is (some #(= 'id %) term-names)))))

;;; ============================================================================
;;; Mixed ASCII and Unicode
;;; ============================================================================

(theory/deftheory MixedCategory
  (type Ob)
  (type → [dom Ob, codom Ob])  ; Unicode type name

  (term compose                ; ASCII term name
    :ctx [a Ob, b Ob, c Ob]
    :args [f (→ a b), g (→ b c)]  ; Unicode in usage
    :ret (→ a c))

  (term id
    :ctx [a Ob]
    :ret (Hom a a)))            ; ASCII Hom also works

(deftest test-mixed-unicode-ascii
  (testing "Can mix unicode and ASCII in same theory"
    (is (core/gat? MixedCategory))
    (is (= 2 (count (:type-constructors MixedCategory))))
    (is (= 2 (count (:term-constructors MixedCategory)))))

  (testing "All forms normalize correctly"
    (let [type-names (map #(-> % :type :head :name) (:type-constructors MixedCategory))]
      (is (some #(= 'Hom %) type-names)))
    (let [term-names (map #(-> % :term :head :name) (:term-constructors MixedCategory))]
      (is (some #(= 'compose %) term-names)))))

;;; ============================================================================
;;; Full Symmetric Monoidal Category with Unicode
;;; ============================================================================

(theory/deftheory UnicodeSMC
  (type Ob)
  (type → [dom Ob, codom Ob])

  (term ⋅
    :ctx [a Ob, b Ob, c Ob]
    :args [f (→ a b), g (→ b c)]
    :ret (→ a c))

  (term id
    :ctx [a Ob]
    :ret (→ a a))

  (term ⊗
    :ctx [a Ob, b Ob]
    :ret Ob)

  (term I
    :ret Ob)

  (term α
    :ctx [a Ob, b Ob, c Ob]
    :ret (→ (⊗ (⊗ a b) c) (⊗ a (⊗ b c))))

  (term λ
    :ctx [a Ob]
    :ret (→ (⊗ I a) a))

  (term ρ
    :ctx [a Ob]
    :ret (→ (⊗ a I) a))

  (term σ
    :ctx [a Ob, b Ob]
    :ret (→ (⊗ a b) (⊗ b a))))

(deftest test-unicode-smc
  (testing "Full SMC with unicode is valid"
    (is (core/gat? UnicodeSMC))
    (is (= 'UnicodeSMC (:name UnicodeSMC))))

  (testing "Has correct number of constructors"
    (is (= 2 (count (:type-constructors UnicodeSMC))))
    (is (= 8 (count (:term-constructors UnicodeSMC)))))

  (testing "All unicode operators normalized to ASCII"
    (let [term-names (map #(-> % :term :head :name) (:term-constructors UnicodeSMC))]
      (is (some #(= 'compose %) term-names))     ; ⋅ → compose
      (is (some #(= 'otimes %) term-names))      ; ⊗ → otimes
      (is (some #(= 'munit %) term-names))       ; I → munit
      (is (some #(= 'associator %) term-names))  ; α → associator
      (is (some #(= 'left-unitor %) term-names)) ; λ → left-unitor
      (is (some #(= 'right-unitor %) term-names)); ρ → right-unitor
      (is (some #(= 'braid %) term-names))))     ; σ → braid

  (testing "Term types use normalized names"
    (let [assoc-term (first (filter #(= 'associator (-> % :term :head :name))
                                    (:term-constructors UnicodeSMC)))
          ret-type (:type (:term assoc-term))]
      ;; Return type should be normalized to Hom, not →
      (is (= 'Hom (-> ret-type :head :name))))))

;;; ============================================================================
;;; Backwards Compatibility
;;; ============================================================================

(theory/deftheory ASCIICategory
  (type Ob)
  (type Hom [dom Ob, codom Ob])

  (term compose
    :ctx [a Ob, b Ob, c Ob]
    :args [f (Hom a b), g (Hom b c)]
    :ret (Hom a c))

  (term id
    :ctx [a Ob]
    :ret (Hom a a)))

(deftest test-ascii-still-works
  (testing "Pure ASCII syntax still works"
    (is (core/gat? ASCIICategory))
    (is (= 2 (count (:type-constructors ASCIICategory))))
    (is (= 2 (count (:term-constructors ASCIICategory)))))

  (testing "ASCII names remain unchanged"
    (let [type-names (map #(-> % :type :head :name) (:type-constructors ASCIICategory))]
      (is (some #(= 'Hom %) type-names)))
    (let [term-names (map #(-> % :term :head :name) (:term-constructors ASCIICategory))]
      (is (some #(= 'compose %) term-names)))))

;;; ============================================================================
;;; Equivalence Tests
;;; ============================================================================

(deftest test-unicode-ascii-equivalence
  (testing "Unicode and ASCII theories are structurally equivalent"
    ;; Same number of types
    (is (= (count (:type-constructors UnicodeCategory))
           (count (:type-constructors ASCIICategory))))

    ;; Same number of terms
    (is (= (count (:term-constructors UnicodeCategory))
           (count (:term-constructors ASCIICategory))))

    ;; Same type names (after normalization)
    (let [unicode-types (set (map #(-> % :type :head :name) (:type-constructors UnicodeCategory)))
          ascii-types (set (map #(-> % :type :head :name) (:type-constructors ASCIICategory)))]
      (is (= unicode-types ascii-types)))

    ;; Same term names (after normalization)
    (let [unicode-terms (set (map #(-> % :term :head :name) (:term-constructors UnicodeCategory)))
          ascii-terms (set (map #(-> % :term :head :name) (:term-constructors ASCIICategory)))]
      (is (= unicode-terms ascii-terms)))))
