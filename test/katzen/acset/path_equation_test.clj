(ns katzen.acset.path-equation-test
  "Path equations (the ACSets.jl `eqs` idiom) declared on a schema's
   `:equations` field desugar into the existing `:axioms` checker."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.acset :as a]
            [katzen.acset.check :as check]))

;; ---------------------------------------------------------------------------
;; The desugaring itself

(deftest path-equation-desugars-to-an-axiom
  (testing "a pair of paths out of :dom becomes a ctx'd term equation"
    (let [ax (check/path-equation->axiom
              {:dom :E :lhs [:src :inv] :rhs [:tgt]})]
      (is (= [{:name 'x :type :E}] (:ctx ax)))
      (is (= '(inv (src x)) (:lhs ax)) "first element of the path is applied first")
      (is (= '(tgt x) (:rhs ax)))))
  (testing "the empty path is the identity"
    (is (= 'x (:lhs (check/path-equation->axiom {:dom :V :lhs [] :rhs [:id]}))))))

;; ---------------------------------------------------------------------------
;; Checking an instance against a path equation
;;
;; Schema: a tiny graph with an edge-reversal `inv : E → E` and src/tgt.
;; Law: src ⨾ ? … we use the classic involution + endpoint-swap laws.

(def graph-schema
  {:name :RevGraph
   :objects [:V :E]
   :homs [{:name :src :dom :E :codom :V}
          {:name :tgt :dom :E :codom :V}
          {:name :inv :dom :E :codom :E}]
   :attr-types []
   :attrs []
   ;; inv swaps endpoints: src(inv e) = tgt e  and  tgt(inv e) = src e
   :equations [{:name 'inv-swaps-src :dom :E :lhs [:inv :src] :rhs [:tgt]}
               {:name 'inv-swaps-tgt :dom :E :lhs [:inv :tgt] :rhs [:src]}]})

(defn- two-vertex-edge [inv-correct?]
  ;; v1, v2; one edge e: v1→v2 and its reverse re: v2→v1.
  ;; inv(e)=re, inv(re)=e. If `inv-correct?` is false we mis-wire inv(e)=e.
  (let [g (a/vector-acset graph-schema)
        [g v1] (a/add-part g :V)
        [g v2] (a/add-part g :V)
        [g e]  (a/add-part g :E)
        [g re] (a/add-part g :E)
        g (-> g (a/set-subpart :src e v1) (a/set-subpart :tgt e v2)
              (a/set-subpart :src re v2) (a/set-subpart :tgt re v1))
        g (-> g (a/set-subpart :inv e (if inv-correct? re e))
              (a/set-subpart :inv re (if inv-correct? e re)))]
    g))

(deftest check-axioms-validates-path-equations
  (testing "a correctly-wired reversal satisfies the endpoint-swap laws"
    (is (nil? (check/check-axioms (two-vertex-edge true))))
    (is (a/acset? (check/check-axioms! (two-vertex-edge true)))
        "strict checker returns the acset on success"))
  (testing "a mis-wired inv (inv e = e) violates src(inv e)=tgt e"
    (let [v (check/check-axioms (two-vertex-edge false))]
      (is (some? v) "violation reported")
      (is (= 'inv-swaps-src (:axiom v)))
      (is (thrown? clojure.lang.ExceptionInfo
                   (check/check-axioms! (two-vertex-edge false)))))))

(deftest equations-and-axioms-are-checked-together
  (testing "schema-axioms concatenates :axioms and compiled :equations"
    (let [schema (assoc graph-schema
                        :axioms [{:name 'manual :ctx [{:name 'x :type :E}]
                                  :lhs '(inv (inv x)) :rhs 'x}])
          all (check/schema-axioms schema)]
      (is (= 3 (count all)) "1 explicit axiom + 2 path equations")
      (is (contains? (set (map :name all)) 'manual))
      (is (contains? (set (map :name all)) 'inv-swaps-src)))))
