(ns katzen.schema-test
  "The canonical standard schemas are valid ACSet schemas, and the rename
   functor binds them to concrete store idents faithfully."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.acset :as a]
            [katzen.schema.knowledge :as kb]
            [katzen.schema.clojure-code :as code]))

(deftest canonical-schemas-are-valid-acset-schemas
  (doseq [s [kb/schema code/schema]]
    (is (a/schema-map? s) (str (:name s) " is a schema map"))
    (testing (str "an empty " (:name s) " ACSet can be built and queried")
      (let [ac (a/vector-acset s)]
        (doseq [o (:objects s)] (is (= 0 (a/nparts ac o))))))))

(deftest knowledge-schema-models-relations-as-junctions
  (testing "the many-valued entity↔entity link is a Link junction (span)"
    (is (some #(= :Link (:dom %)) (:homs kb/schema)))
    (is (= #{:link-src :link-dst}
           (set (map :name (filter #(= :Link (:dom %)) (:homs kb/schema))))))
    (is (= :Identity (:codom (a/attr-by-name kb/schema kb/identity-attr))))))

(deftest rename-binds-abstract-names-to-store-idents
  (testing "rename-schema renames objects, attr-types, morphisms, dom/codom"
    (let [bound (a/rename-schema kb/schema
                  {:title :entity/title :employer :entity/employer
                   :link-src :link/src :link-dst :link/dst :kind :entity/type})]
      (is (a/schema-map? bound))
      (is (= :entity/title (:name (a/attr-by-name bound :entity/title))))
      (is (nil? (a/attr-by-name bound :title)) "old name gone")
      (let [employer (a/hom-by-name bound :entity/employer)]
        (is (= :Entity (:dom employer)))
        (is (= :Entity (:codom employer))))
      (testing "names not in the map are left as-is"
        (is (= :Identity (:codom (a/attr-by-name bound :entity/title))))
        (is (nil? (a/attr-by-name bound :entity/summary)) "summary not in map → not renamed")
        (is (some #(= :summary (:name %)) (:attrs bound)) "summary stays :summary"))))
  (testing "renaming is an iso: round-trip restores the original"
    (let [m {:title :entity/title :employer :entity/employer}
          inv (clojure.set/map-invert m)]
      (is (= kb/schema (a/rename-schema (a/rename-schema kb/schema m) inv))))))

(deftest code-schema-promotes-the-dvergr-index-shape
  (testing "Def keyed by qname Identity + a Ref junction"
    (is (= :Identity (:codom (a/attr-by-name code/schema :qname))))
    (is (= {:name :from :dom :Ref :codom :Def} (a/hom-by-name code/schema :from)))
    (is (= :Identity (:codom (a/attr-by-name code/schema :to))))))
