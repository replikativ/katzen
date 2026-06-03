(ns katzen.acset.cardinality-test
  "The datahike backend honours :cardinality :many (native datahike many — no
   junction object) and :unique, so katzen is a lens over what datahike already
   does rather than a reimplementation."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [katzen.acset :as a]
            [katzen.acset.datahike :as kdh]))

(def schema
  {:name :Code
   :objects [:Def]
   :homs []
   :attr-types [:Identity :String]
   :attrs [{:name :def/qname :dom :Def :codom :Identity :unique :db.unique/identity}
           {:name :def/refs  :dom :Def :codom :Identity :cardinality :many}]})

(deftest many-cardinality-is-native-not-a-junction
  (let [ac (kdh/datahike-acset schema)
        [ac d1] (a/add-part ac :Def)]
    (a/set-subpart ac :def/qname d1 "demo/a")
    (a/set-subpart ac :def/refs  d1 #{"clojure.core/atom" "clojure.core/str"})
    (testing "a :many morphism's subpart is the SET of values"
      (is (= #{"clojure.core/atom" "clojure.core/str"} (a/subpart ac :def/refs d1))))
    (testing "a :one morphism's subpart is the single value"
      (is (= "demo/a" (a/subpart ac :def/qname d1))))
    (testing "set-subpart REPLACES the set for :many"
      (a/set-subpart ac :def/refs d1 #{"clojure.core/atom"})
      (is (= #{"clojure.core/atom"} (a/subpart ac :def/refs d1))))
    (testing "incident over a :many morphism (inverse image / find-references)"
      (let [[ac d2] (a/add-part ac :Def)]
        (a/set-subpart ac :def/qname d2 "demo/b")
        (a/set-subpart ac :def/refs  d2 #{"clojure.core/atom"})
        (is (= #{d1 d2} (set (a/incident ac :def/refs "clojure.core/atom"))))))
    (testing "subpart-all maps part → set for :many"
      (is (every? set? (vals (a/subpart-all ac :def/refs)))))))

(deftest declared-unique-is-installed-natively
  (let [ac (kdh/datahike-acset schema)]
    (is (= :db.unique/identity (:db/unique (d/entity (d/db (:conn ac)) :def/qname)))
        "the schema's :unique becomes datahike's native :db/unique")
    (testing "the native unique constraint is enforced on the data"
      (let [[ac d1] (a/add-part ac :Def)
            _  (a/set-subpart ac :def/qname d1 "x")
            [ac d2] (a/add-part ac :Def)]
        (is (thrown? Throwable (a/set-subpart ac :def/qname d2 "x"))
            "two distinct Defs cannot share a unique qname")))))
