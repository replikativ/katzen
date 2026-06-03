(ns katzen.model-simple-test
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.model :as model]
            [katzen.theory :as theory]))

(theory/deftheory TestTh
  (type Ob)
  (term check :args [x Ob] :ret Ob))

(model/definstance TestModel TestTh {}
  (Ob [_m args]
    ;; Ob is nullary, args should be empty
      true)

  (check [_m args]
         (let [[x] args]
           (pos? x))))

(deftest test-simple-model
  (testing "Simple model works"
    (let [m (->TestModel)]
      (is (model/concrete-model? m))
      (is (Ob m))  ; nullary
      (is (check m 1))
      (is (not (check m 0))))))
