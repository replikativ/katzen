(ns katzen.program-test
  "The Clojure-program → string-diagram functor (`katzen.program`). The diagram
   must expose the dataflow structure: variable reuse as FAN-OUT (copy), an unused
   binding as a DROPPED box (delete), and pure ops with the cartesian bead."
  (:require [clojure.test :refer [deftest is testing]]
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

(deftest renders-mermaid
  (let [m (prog/fn->mermaid stats-form)]
    (is (re-find #"flowchart LR" m))
    (is (re-find #"\[\[" m) "pure boxes use the doubled-border node")
    (is (re-find #"RESULT" m) "a result node is emitted")))
