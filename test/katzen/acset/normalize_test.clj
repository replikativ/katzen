(ns katzen.acset.normalize-test
  "Tests for the directed-rewrite normalizer over schema-axiom-style
   s-expressions. Three classes of test:

   1. Rule orientation — size-decreasing axioms become rules; same-size
      ones report as non-orientable.
   2. Single-axiom normalization — the involution case and its powers.
   3. Multi-axiom interaction — `src(inv e) = tgt e` and friends compose
      correctly with `inv ∘ inv = id`."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.acset.graphs :as gg]
            [katzen.acset.normalize :as n]))

;; ============================================================================
;; Test schemas
;; ============================================================================

(def SchInvolution
  (assoc gg/SchSymmetricGraph
         :name 'SchInvolution
         :axioms [{:name 'inv-involution
                   :ctx [{:name 'e :type :E}]
                   :lhs '(inv (inv e))
                   :rhs 'e}]))

(def SchSymFull
  (assoc gg/SchSymmetricGraph
         :name 'SchSymFull
         :axioms
         [{:name 'inv-involution :ctx [{:name 'e :type :E}]
           :lhs '(inv (inv e))   :rhs 'e}
          {:name 'src-of-inv     :ctx [{:name 'e :type :E}]
           :lhs '(src (inv e))   :rhs '(tgt e)}
          {:name 'tgt-of-inv     :ctx [{:name 'e :type :E}]
           :lhs '(tgt (inv e))   :rhs '(src e)}]))

;; ============================================================================
;; Orientation
;; ============================================================================

(deftest test-involution-orients-large-to-small
  (testing "(inv (inv e)) = e is a size-decreasing rewrite"
    (let [[rule] (n/theory-rules SchInvolution)]
      (is (= '(inv (inv e)) (:lhs rule)))
      (is (= 'e (:rhs rule)))
      (is (= #{'e} (:vars rule))))))

(deftest test-flipped-orientation-when-rhs-larger
  (testing "An axiom written 'small = big' gets flipped to 'big → small'"
    (let [schema {:objects [:X] :homs [{:name :f :dom :X :codom :X}]
                  :axioms [{:name 'flipped
                            :ctx [{:name 'x :type :X}]
                            :lhs 'x
                            :rhs '(f (f x))}]}
          [rule] (n/theory-rules schema)]
      (is (= '(f (f x)) (:lhs rule)))
      (is (= 'x (:rhs rule)))
      (is (= :axiom-flipped (:origin rule))))))

(deftest test-non-orientable-axiom-skipped
  (testing "An axiom whose sides have equal node count is dropped"
    (let [schema {:objects [:X]
                  :homs [{:name :m :dom :X :codom :X}]
                  ;; The classic non-orientable: associativity-shaped equation
                  :axioms [{:name 'symmetric-equality
                            :ctx [{:name 'a :type :X} {:name 'b :type :X}]
                            :lhs '(m a b)
                            :rhs '(m b a)}]}
          report (n/axiom-orientation-report schema)]
      (is (= 0 (:orientable report)))
      (is (= ['symmetric-equality] (:non-orientable report))))))

(deftest test-orientation-report
  (let [report (n/axiom-orientation-report SchSymFull)]
    (is (= 3 (:total report)))
    (is (= 3 (:orientable report)))
    (is (empty? (:non-orientable report)))))

;; ============================================================================
;; Single-axiom normalization: involution and its powers
;; ============================================================================

(deftest test-double-inv-collapses
  (is (= 'e (n/normalize SchInvolution '(inv (inv e))))))

(deftest test-triple-inv-collapses-to-single
  (is (= '(inv e) (n/normalize SchInvolution '(inv (inv (inv e)))))))

(deftest test-quadruple-inv-collapses-to-zero
  (is (= 'e (n/normalize SchInvolution '(inv (inv (inv (inv e))))))))

(deftest test-inv-of-non-inv-leaves-alone
  (testing "Lone (inv …) is irreducible"
    (is (= '(inv e) (n/normalize SchInvolution '(inv e))))))

(deftest test-double-inv-inside-other-op
  (testing "Bottom-up rewriting fires inside heads of other operations"
    (is (= '(src e) (n/normalize SchInvolution '(src (inv (inv e))))))))

(deftest test-leaf-term-unchanged
  (is (= 'e (n/normalize SchInvolution 'e))))

;; ============================================================================
;; Multi-axiom interaction
;; ============================================================================

(deftest test-src-of-inv-fires
  (is (= '(tgt e) (n/normalize SchSymFull '(src (inv e))))))

(deftest test-involution-then-tgt-of-inv
  (testing "(tgt (inv (inv e))) — involution first, then nothing left to do"
    (is (= '(tgt e) (n/normalize SchSymFull '(tgt (inv (inv e))))))))

(deftest test-nested-rewrites-compose
  (testing "(src (inv (inv (inv e)))) — involution shrinks twice, then src-of-inv"
    (is (= '(tgt e) (n/normalize SchSymFull '(src (inv (inv (inv e)))))))))

(deftest test-src-inv-binding-captures-subterm
  (testing "(src (inv (src (inv e)))) — outer pattern captures (tgt e) after
            inner rewrite"
    (is (= '(tgt (tgt e))
           (n/normalize SchSymFull '(src (inv (src (inv e)))))))))

;; ============================================================================
;; equiv?
;; ============================================================================

(deftest test-equiv-self
  (is (n/equiv? SchInvolution 'e 'e)))

(deftest test-equiv-double-inv
  (is (n/equiv? SchInvolution '(inv (inv e)) 'e)))

(deftest test-not-equiv-distinct-bindings
  (is (not (n/equiv? SchInvolution '(inv e1) '(inv e2)))))

(deftest test-equiv-mixed-rules
  (is (n/equiv? SchSymFull '(src (inv (inv e))) '(src e)))
  (is (n/equiv? SchSymFull '(src (inv e)) '(tgt e))))

(deftest test-equiv-noop-schema
  (testing "A schema with no axioms gives `equiv? ↔ =`"
    (let [schema {:objects [] :homs [] :axioms []}]
      (is (n/equiv? schema 'x 'x))
      (is (not (n/equiv? schema 'x 'y))))))

;; ============================================================================
;; Canonical-form hints (associativity)
;; ============================================================================

(def SchMonoid
  "Monoid-style schema with assoc + unit axioms; assoc hinted as :lhs
   canonical (= left-associative)."
  {:name 'SchMonoid
   :objects [:El]
   :homs []
   :axioms [{:name 'assoc
             :ctx [{:name 'x :type :El} {:name 'y :type :El} {:name 'z :type :El}]
             :lhs '(mul (mul x y) z)
             :rhs '(mul x (mul y z))
             :canonical :lhs}
            {:name 'unit-left :ctx [{:name 'x :type :El}]
             :lhs '(mul u x) :rhs 'x}
            {:name 'unit-right :ctx [{:name 'x :type :El}]
             :lhs '(mul x u) :rhs 'x}]})

(deftest test-canonical-hint-makes-assoc-orientable
  (testing "With :canonical :lhs the same-size assoc axiom becomes a rule"
    (let [report (n/axiom-orientation-report SchMonoid)]
      (is (= 3 (:total report)))
      (is (= 3 (:orientable report))))))

(deftest test-rewrites-right-assoc-to-left
  (is (= '(mul (mul a b) c)
         (n/normalize SchMonoid '(mul a (mul b c))))))

(deftest test-deep-right-nested-cascades-to-left
  (is (= '(mul (mul (mul a b) c) d)
         (n/normalize SchMonoid '(mul a (mul b (mul c d)))))))

(deftest test-already-left-assoc-is-fixed-point
  (let [t '(mul (mul (mul a b) c) d)]
    (is (= t (n/normalize SchMonoid t)))))

(deftest test-mixed-shape-canonicalizes
  (testing "(mul a (mul (mul b c) d)) — middle is already left-assoc but
            outer needs re-assoc; both must compose to fully left-assoc"
    (is (= '(mul (mul (mul a b) c) d)
           (n/normalize SchMonoid '(mul a (mul (mul b c) d)))))))

(deftest test-assoc-and-unit-rules-compose
  (testing "Mixing unit elimination with associativity"
    (is (= '(mul a b)
           (n/normalize SchMonoid '(mul u (mul a (mul u b))))))
    (is (= '(mul a b)
           (n/normalize SchMonoid '(mul (mul a u) (mul b u)))))))

(deftest test-rhs-canonical-orientation
  (testing "`:canonical :rhs` produces the opposite orientation"
    (let [schema (update SchMonoid :axioms
                         (fn [axs] (mapv (fn [ax]
                                           (if (= 'assoc (:name ax))
                                             (assoc ax :canonical :rhs)
                                             ax))
                                         axs)))]
      ;; Now (mul (mul a b) c) → (mul a (mul b c)) (right-assoc).
      (is (= '(mul a (mul b c))
             (n/normalize schema '(mul (mul a b) c)))))))
