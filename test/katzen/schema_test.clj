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

(deftest knowledge-schema-uses-native-cardinality-many
  (testing "entity↔entity links are a native :cardinality :many hom (no junction)"
    (let [links (a/hom-by-name kb/schema :links)]
      (is (= :many (:cardinality links)))
      (is (= :Entity (:dom links))) (is (= :Entity (:codom links))))
    (is (not (some #{:Link} (:objects kb/schema))) "no Link junction object")
    (is (= :Identity (:codom (a/attr-by-name kb/schema kb/identity-attr))))
    (is (= :db.unique/value (:unique (a/attr-by-name kb/schema :title))) "title is unique")))

(deftest rename-binds-abstract-names-to-store-idents
  (testing "rename-schema renames objects, attr-types, morphisms, dom/codom"
    (let [bound (a/rename-schema kb/schema
                                 {:title :entity/title :links :entity/links :kind :entity/type})]
      (is (a/schema-map? bound))
      (is (= :entity/title (:name (a/attr-by-name bound :entity/title))))
      (is (nil? (a/attr-by-name bound :title)) "old name gone")
      (let [links (a/hom-by-name bound :entity/links)]
        (is (= :Entity (:dom links)))
        (is (= :Entity (:codom links)))
        (is (= :many (:cardinality links)) "cardinality carries through rename"))
      (testing "names not in the map are left as-is"
        (is (= :Identity (:codom (a/attr-by-name bound :entity/title))))
        (is (nil? (a/attr-by-name bound :entity/summary)) "summary not in map → not renamed")
        (is (some #(= :summary (:name %)) (:attrs bound)) "summary stays :summary"))))
  (testing "renaming is an iso: round-trip restores the original"
    (let [m {:title :entity/title :links :entity/links}
          inv (clojure.set/map-invert m)]
      (is (= kb/schema (a/rename-schema (a/rename-schema kb/schema m) inv))))))

(deftest merge-schema-extends-a-base-with-domain-fields
  (testing "a consumer extends the generic KB schema with its own morphisms"
    (let [ext   {:homs [{:name :employer :dom :Entity :codom :Entity}]
                 :attr-types [:Long]
                 :attrs [{:name :mention-count :dom :Entity :codom :Long}]}
          merged (a/merge-schema kb/schema ext)]
      (is (= :Entity (:codom (a/hom-by-name merged :employer))) "extension hom present")
      (is (= :Long (:codom (a/attr-by-name merged :mention-count))) "extension attr present")
      (is (some #(= :title (:name %)) (:attrs merged)) "base attrs retained")
      (is (some #(= :links (:name %)) (:homs merged)) "base homs retained")
      (is (= [:Identity :String :Keyword :Instant :Long] (:attr-types merged))
          "attr-types unioned, base order preserved")))
  (testing "merge is name-deduped (base wins) — re-extending changes nothing"
    (is (= (a/merge-schema kb/schema kb/schema) kb/schema))))

(deftest code-schema-uses-native-many-refs
  (testing "Def keyed by qname Identity + a :cardinality :many refs attr (no Ref junction)"
    (is (= :Identity (:codom (a/attr-by-name code/schema :qname))))
    (is (not (some #{:Ref} (:objects code/schema))) "no Ref junction object")
    (let [refs (a/attr-by-name code/schema :refs)]
      (is (= :many (:cardinality refs)))
      (is (= :Identity (:codom refs))))))
