(ns katzen.program-test
  "The Clojure-program → string-diagram functor (`katzen.program`). The diagram
   must expose the dataflow structure: variable reuse as FAN-OUT (copy), an unused
   binding as a DROPPED box (delete), and pure ops with the cartesian bead."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.set]
            [katzen.diagram :as diagram]
            [katzen.program :as prog]))

(def ^:private stats-form
  '(defn stats [xs]
     (let [n      (count xs)
           total  (reduce + 0 xs)
           mean   (/ total n)
           unused (first xs)]
       (vector mean n))))

(defn- box-by-label [g label] (first (filter #(= label (:label %)) (:boxes g))))
(defn- out-degree [g port] (count (filter #(= port (:from %)) (:wires g))))
(defn- consumers   [g port] (count (filter #(= port (:from %)) (:wires g))))

(deftest exposes-dataflow-structure
  (let [g (prog/fn->diagram stats-form)]
    (testing "the function's parameter is an input"
      (is (= ["xs"] (mapv :label (:inputs g)))))
    (testing "variable reuse = fan-out (copy): n is used by both / and vector"
      (let [n (:out (box-by-label g "count"))]
        (is (= 2 (out-degree g n)) "n's port forks to two consumers")))
    (testing "an unused binding = a dropped box (delete): (first xs) has no consumer"
      (let [u (:out (box-by-label g "first"))]
        (is (= 0 (consumers g u)) "the unused binding's port feeds nothing")))
    (testing "pure ops carry the cartesian bead"
      (is (every? :pure? (filter #(#{"count" "reduce" "/" "vector"} (:label %)) (:boxes g)))))
    (testing "there is a single output wired from the final form"
      (is (= 1 (count (:outputs g))))
      (is (= (:out (box-by-label g "vector")) (first (:outputs g)))))))

(deftest impure-ops-lose-the-bead
  (let [g (prog/fn->diagram '(defn poke [a] (let [x (atom a)] (reset! x 9) (deref x))))]
    (is (false? (:pure? (box-by-label g "atom"))))
    (is (false? (:pure? (box-by-label g "reset!"))))
    (is (false? (:pure? (box-by-label g "deref"))))))

(def ^:private classify-form
  '(defn classify [x]
     (let [a (abs x)]
       (if (pos? x)
         (str "pos:" a)
         (vector (- a) x)))))

(deftest conditionals-become-a-cond-box-with-nested-branches
  (let [g (prog/fn->diagram classify-form)
        cond-box (first (filter #(= :cond (:kind %)) (:boxes g)))
        cid (:id cond-box)]
    (testing "an if produces a :cond box"
      (is (some? cond-box))
      (is (= "if" (:label cond-box))))
    (testing "branches are walked into grouped sub-diagrams (the operadic fill)"
      (let [groups (set (map :group (:boxes g)))]
        (is (contains? groups [[cid :then]]) "a then sub-region")
        (is (contains? groups [[cid :else]]) "an else sub-region")))
    (testing "the condition and both branch outputs wire into the cond box (selection)"
      (let [into-cond (->> (:wires g) (filter #(= (:out cond-box) (:to %))) count)]
        (is (>= into-cond 3) "condition + 2 branch outputs feed the selection")))
    (testing "a shared in-scope value flows into both branches"
      (let [a-port (:out (box-by-label g "abs"))
            then-boxes (set (map :id (filter #(= [[cid :then]] (:group %)) (:boxes g))))
            else-boxes (set (map :id (filter #(= [[cid :else]] (:group %)) (:boxes g))))
            p->b (into {} (for [b (:boxes g) :when (:out b)] [(:out b) (:id b)]))
            targets (->> (:wires g) (filter #(= a-port (:from %))) (map #(p->b (:to %))) set)]
        (is (seq (clojure.set/intersection targets then-boxes)))
        (is (seq (clojure.set/intersection targets else-boxes)))))))

(deftest higher-order-via-run-and-program-codes
  (testing "a fn literal is a :program code box ⌜·⌝ with its body nested"
    (let [g (prog/fn->diagram '(defn sq [n] ((fn [x] (* x x)) n)))
          pbox (first (filter #(= :program (:kind %)) (:boxes g)))]
      (is (some? pbox))
      (is (= "fn" (:label pbox)))
      (is (some #(= [[(:id pbox) :body]] (:group %)) (:boxes g)) "body in a nested group")
      (is (some #(= :run (:kind %)) (:boxes g)) "applied via a run box")))
  (testing "a bare fn ref passed as a value is a :program code box (⌜inc⌝)"
    (let [g (prog/fn->diagram '(defn f [xs] (map inc xs)))]
      (is (some #(and (= :program (:kind %)) (= "inc" (:label %))) (:boxes g)))))
  (testing "applying a local fn value emits a :run/apply box (Dusko: {g} a)"
    (let [g (prog/fn->diagram '(defn ap [g a] (g a)))]
      (is (= 1 (count (filter #(= :run (:kind %)) (:boxes g))))))))

(deftest recursion-renders-as-a-trace-loop
  (testing "self-recursion: a self-call is a :recursive box with feedback (trace) edges to the params"
    (let [g (prog/fn->diagram '(defn fact [n acc] (if (zero? n) acc (fact (dec n) (* n acc)))))]
      (is (some #(= :recursive (:kind %)) (:boxes g)))
      (is (= 2 (count (filter :trace? (:wires g)))) "both args feed back to the two params")))
  (testing "loop/recur: recur feeds back to the loop vars via trace edges"
    (let [g (prog/fn->diagram '(defn cnt [xs]
                                 (loop [ys xs n 0]
                                   (if (seq ys) (recur (rest ys) (inc n)) n))))]
      (is (some #(= :recur (:kind %)) (:boxes g)))
      (is (= 2 (count (filter :trace? (:wires g)))) "both loop vars fed back"))))

(deftest reactflow-emits-nested-collapsible-graph
  (let [rf    (diagram/->reactflow (prog/fn->diagram classify-form))
        nodes (:nodes rf)
        groups (filter #(= "group" (:type %)) nodes)
        label= (fn [l] (first (filter #(= l (get-in % [:data :label])) nodes)))]
    (testing "cond branches → collapsible group container nodes"
      (is (= 2 (count groups)))
      (is (= #{"then" "else"} (set (map #(get-in % [:data :label]) groups)))))
    (testing "branch boxes nest under a group via parentId"
      (let [gids (set (map :id groups))]
        (is (some #(and (= "box" (:type %)) (contains? gids (:parentId %))) nodes))))
    (testing "the cond box is typed, and variable reuse is flagged as fan-out (copy)"
      (is (some #(= "cond" (:type %)) nodes))
      (is (> (get-in (label= "abs") [:data :fanout]) 1) "abs/a is reused → fan-out > 1"))
    (testing "a result node + edges into it"
      (is (some #(= "result" (:type %)) nodes))
      (is (some #(= "RESULT" (:target %)) (:edges rf))))
    (testing "parent group nodes precede their children (ReactFlow requirement)"
      (let [order (zipmap (map :id nodes) (range))]
        (is (every? (fn [n] (or (nil? (:parentId n)) (< (order (:parentId n)) (order (:id n)))))
                    nodes))))))

(deftest renders-mermaid
  (let [m (prog/fn->mermaid stats-form)]
    (is (re-find #"flowchart LR" m))
    (is (re-find #"\[\[" m) "pure boxes use the doubled-border node")
    (is (re-find #"RESULT" m) "a result node is emitted")))
