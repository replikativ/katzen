(ns katzen.eval-test
  "The type-side evaluator: term evaluation (morphism navigation + type-side
   operations), computed properties, validation predicates, and monoid rollups."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.acset :as a]
            [katzen.eval :as ev]
            [katzen.aggregate :as agg]))

;; A tiny invoicing schema: Items point at an Invoice (junction fan-in), each
;; with an amount; entities carry a title + a numeric mention-count.
(def schema
  {:name :Invoicing
   :objects [:Invoice :Item]
   :homs [{:name :invoice :dom :Item :codom :Invoice}]
   :attr-types [:String :Long]
   :attrs [{:name :inv-no  :dom :Invoice :codom :String}
           {:name :amount  :dom :Item    :codom :Long}
           {:name :label   :dom :Item    :codom :String}]
   :equations []})

(defn- sample []
  (let [a0 (a/vector-acset schema)
        [a inv] (a/add-part a0 :Invoice)
        a (a/set-subpart a :inv-no inv "INV-1")
        mk (fn [a amt lbl]
             (let [[a it] (a/add-part a :Item)]
               (-> a (a/set-subpart :invoice it inv)
                     (a/set-subpart :amount it amt)
                     (a/set-subpart :label it lbl))))
        a (-> a (mk 100 "widget") (mk 250 "gadget") (mk 50 "bolt"))]
    {:acset a :inv inv}))

(deftest eval-term-navigates-and-computes
  (let [{:keys [acset]} (sample)
        item (first (a/parts acset :Item))]
    (testing "type-side operation"
      (is (= 6 (ev/eval-term acset ev/base-model {} '(+ 1 2 3))))
      (is (= "ab!" (ev/eval-term acset ev/base-model {} '(str "ab" "!")))))
    (testing "context variable + morphism navigation"
      (is (= "INV-1" (ev/eval-term acset ev/base-model {'x item}
                                   '(inv-no (invoice x)))) "follow item→invoice→inv-no"))
    (testing "nested op over navigated values"
      (is (= "item:widget" (ev/eval-term acset ev/base-model {'x item}
                                         '(str "item:" (label x))))))
    (testing "nil propagates through morphism navigation"
      (let [[a orphan] (a/add-part acset :Item)]  ; no :invoice set
        (is (nil? (ev/eval-term a ev/base-model {'x orphan} '(invoice x))))))))

(deftest computed-and-validation-properties
  (let [{:keys [acset]} (sample)]
    (testing "computed property: a derived label per Item"
      (let [prop {:dom :Item :var 'x :term '(str (label x) " ($" (amount x) ")")}
            view (ev/derived-all acset prop)]
        (is (= #{"widget ($100)" "gadget ($250)" "bolt ($50)"} (set (vals view))))))
    (testing "validation: amounts must be positive"
      (is (ev/valid? acset {:dom :Item :var 'x :pred '(pos? (amount x))}))
      (let [[bad it] (a/add-part acset :Item)
            bad (a/set-subpart bad :amount it -5)]
        (is (not (ev/valid? bad {:dom :Item :var 'x :pred '(pos? (amount x))})))
        (is (= [it] (map :part (ev/invalid bad {:dom :Item :var 'x
                                                :pred '(pos? (amount x))}))))))))

(deftest monoid-rollups
  (let [{:keys [acset inv]} (sample)]
    (testing "sum of item amounts over the invoice (junction fan-in)"
      (is (= 400 (agg/rollup-attr acset :sum :invoice :amount inv))))
    (testing "count / max / min"
      (is (= 3   (agg/rollup-attr acset :count :invoice :amount inv)))
      (is (= 250 (agg/rollup-attr acset :max   :invoice :amount inv)))
      (is (= 50  (agg/rollup-attr acset :min   :invoice :amount inv))))
    (testing "rollup of a computed term (amount with tax), not a stored column"
      (is (= 440 (agg/rollup-term acset :sum :invoice
                                  {:var 'x :term '(+ (amount x) (quot (amount x) 10))} inv))
          "each amount + 10% then summed: 110+275+55"))
    (testing "fold is the bare monoid aggregate"
      (is (= 15 (agg/fold :sum [1 2 3 4 5]))))))
