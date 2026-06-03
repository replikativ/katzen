(ns katzen.scope-test
  "Comprehensive tests for scope tracking functionality."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.scope :as scope]))

;;; ============================================================================
;;; Basic Construction Tests
;;; ============================================================================

(deftest test-scope-tag-creation
  (testing "Can create scope tags"
    (let [tag1 (scope/scope-tag)
          tag2 (scope/scope-tag)]
      (is (scope/scope-tag? tag1))
      (is (scope/scope-tag? tag2))
      (is (not= (:uuid tag1) (:uuid tag2))
          "Different tags should have different UUIDs")))

  (testing "Can create scope tag with specific UUID"
    (let [uuid (java.util.UUID/randomUUID)
          tag (scope/scope-tag uuid)]
      (is (= uuid (:uuid tag))))))

(deftest test-ident-creation
  (testing "Can create idents"
    (let [tag (scope/scope-tag)
          ident (scope/ident tag 0 'x)]
      (is (scope/gat-ident? ident))
      (is (= tag (:tag ident)))
      (is (= 0 (:lid ident)))
      (is (= 'x (:name ident)))))

  (testing "Can create anonymous idents"
    (let [tag (scope/scope-tag)
          ident (scope/ident tag 0 nil)]
      (is (nil? (:name ident)))
      (is (= 0 (:lid ident)))))

  (testing "Ident construction validates inputs"
    (let [tag (scope/scope-tag)]
      (is (thrown? AssertionError
                   (scope/ident "not-a-tag" 0 'x)))
      (is (thrown? AssertionError
                   (scope/ident tag "not-an-int" 'x)))
      (is (thrown? AssertionError
                   (scope/ident tag 0 "not-a-symbol"))))))

;;; ============================================================================
;;; Scope Context Tests
;;; ============================================================================

(deftest test-scope-context-creation
  (testing "Can create empty scope context"
    (let [ctx (scope/scope-context)]
      (is (instance? katzen.scope.ScopeContext ctx))
      (is (scope/scope-tag? (:tag ctx)))
      (is (= 0 (:next-lid ctx)))
      (is (empty? (:bindings ctx)))))

  (testing "Can create context with specific tag"
    (let [tag (scope/scope-tag)
          ctx (scope/scope-context tag)]
      (is (= tag (:tag ctx))))))

(deftest test-bind
  (testing "Can bind single name"
    (let [ctx (scope/scope-context)
          [ctx2 ident] (scope/bind ctx 'x)]
      (is (= 'x (:name ident)))
      (is (= 0 (:lid ident)))
      (is (= 1 (:next-lid ctx2)))
      (is (= ident (get-in ctx2 [:bindings 'x])))))

  (testing "Multiple binds increment lid"
    (let [ctx (scope/scope-context)
          [ctx2 x-ident] (scope/bind ctx 'x)
          [ctx3 y-ident] (scope/bind ctx2 'y)]
      (is (= 0 (:lid x-ident)))
      (is (= 1 (:lid y-ident)))
      (is (= 2 (:next-lid ctx3)))))

  (testing "Can shadow names"
    (let [ctx (scope/scope-context)
          [ctx2 x1] (scope/bind ctx 'x)
          [ctx3 x2] (scope/bind ctx2 'x)]
      (is (not= x1 x2))
      (is (= 0 (:lid x1)))
      (is (= 1 (:lid x2)))
      (is (= x2 (scope/lookup ctx3 'x))
          "Lookup should return most recent binding"))))

(deftest test-bind-many
  (testing "Can bind multiple names at once"
    (let [ctx (scope/scope-context)
          [ctx2 idents] (scope/bind-many ctx ['a 'b 'c])]
      (is (= 3 (count idents)))
      (is (= ['a 'b 'c] (mapv :name idents)))
      (is (= [0 1 2] (mapv :lid idents)))
      (is (= 3 (:next-lid ctx2))))))

(deftest test-lookup
  (testing "Can lookup bound names"
    (let [ctx (scope/scope-context)
          [ctx2 x] (scope/bind ctx 'x)
          [ctx3 y] (scope/bind ctx2 'y)]
      (is (= x (scope/lookup ctx2 'x)))
      (is (= y (scope/lookup ctx3 'y)))
      (is (= x (scope/lookup ctx3 'x))
          "Earlier bindings still accessible")))

  (testing "Lookup returns nil for unbound names"
    (let [ctx (scope/scope-context)]
      (is (nil? (scope/lookup ctx 'unbound)))))

  (testing "has-binding? works correctly"
    (let [ctx (scope/scope-context)
          [ctx2 _] (scope/bind ctx 'x)]
      (is (not (scope/has-binding? ctx 'x)))
      (is (scope/has-binding? ctx2 'x)))))

;;; ============================================================================
;;; retag Tests
;;; ============================================================================

(deftest test-retag-ident
  (testing "retag changes ident's scope tag"
    (let [tag1 (scope/scope-tag)
          tag2 (scope/scope-tag)
          ident (scope/ident tag1 0 'x)
          retagged (scope/retag ident {tag1 tag2})]
      (is (= tag2 (:tag retagged)))
      (is (= 0 (:lid retagged)))
      (is (= 'x (:name retagged)))))

  (testing "retag leaves ident unchanged if tag not in map"
    (let [tag1 (scope/scope-tag)
          tag2 (scope/scope-tag)
          ident (scope/ident tag1 0 'x)
          retagged (scope/retag ident {tag2 (scope/scope-tag)})]
      (is (= ident retagged)))))

(deftest test-retag-recursive-structures
  (testing "retag works on vectors"
    (let [tag1 (scope/scope-tag)
          tag2 (scope/scope-tag)
          i1 (scope/ident tag1 0 'x)
          i2 (scope/ident tag1 1 'y)
          vec [i1 i2]
          retagged (scope/retag vec {tag1 tag2})]
      (is (= tag2 (-> retagged first :tag)))
      (is (= tag2 (-> retagged second :tag)))))

  (testing "retag works on maps"
    (let [tag1 (scope/scope-tag)
          tag2 (scope/scope-tag)
          i1 (scope/ident tag1 0 'x)
          i2 (scope/ident tag1 1 'y)
          m {i1 i2}
          retagged (scope/retag m {tag1 tag2})]
      (is (= tag2 (-> retagged keys first :tag)))
      (is (= tag2 (-> retagged vals first :tag)))))

  (testing "retag works on nested structures"
    (let [tag1 (scope/scope-tag)
          tag2 (scope/scope-tag)
          i (scope/ident tag1 0 'x)
          nested {:foo [i {:bar i}]}
          retagged (scope/retag nested {tag1 tag2})]
      (is (= tag2 (-> retagged :foo first :tag)))
      (is (= tag2 (-> retagged :foo second :bar :tag))))))

;;; ============================================================================
;;; rename Tests
;;; ============================================================================

(deftest test-rename-ident
  (testing "rename changes ident's name within scope"
    (let [tag (scope/scope-tag)
          ident (scope/ident tag 0 'x)
          renamed (scope/rename ident tag {'x 'y})]
      (is (= 'y (:name renamed)))
      (is (= tag (:tag renamed)))
      (is (= 0 (:lid renamed)))))

  (testing "rename only affects specified scope"
    (let [tag1 (scope/scope-tag)
          tag2 (scope/scope-tag)
          ident (scope/ident tag1 0 'x)
          renamed (scope/rename ident tag2 {'x 'y})]
      (is (= ident renamed)
          "Should not rename if tag doesn't match")))

  (testing "rename leaves ident unchanged if name not in map"
    (let [tag (scope/scope-tag)
          ident (scope/ident tag 0 'x)
          renamed (scope/rename ident tag {'y 'z})]
      (is (= ident renamed)))))

(deftest test-rename-recursive-structures
  (testing "rename works on vectors"
    (let [tag (scope/scope-tag)
          i1 (scope/ident tag 0 'x)
          i2 (scope/ident tag 1 'y)
          vec [i1 i2]
          renamed (scope/rename vec tag {'x 'a 'y 'b})]
      (is (= 'a (-> renamed first :name)))
      (is (= 'b (-> renamed second :name)))))

  (testing "rename preserves non-matching names"
    (let [tag (scope/scope-tag)
          i1 (scope/ident tag 0 'x)
          i2 (scope/ident tag 1 'y)
          vec [i1 i2]
          renamed (scope/rename vec tag {'x 'a})]
      (is (= 'a (-> renamed first :name)))
      (is (= 'y (-> renamed second :name))
          "y should be unchanged"))))

;;; ============================================================================
;;; reident Tests
;;; ============================================================================

(deftest test-reident
  (testing "reident replaces idents completely"
    (let [tag1 (scope/scope-tag)
          tag2 (scope/scope-tag)
          i1 (scope/ident tag1 0 'x)
          i2 (scope/ident tag2 5 'y)
          reidentified (scope/reident i1 {i1 i2})]
      (is (= i2 reidentified))))

  (testing "reident leaves ident unchanged if not in map"
    (let [tag (scope/scope-tag)
          i1 (scope/ident tag 0 'x)
          i2 (scope/ident tag 1 'y)
          reidentified (scope/reident i1 {i2 (scope/ident tag 2 'z)})]
      (is (= i1 reidentified))))

  (testing "reident works on vectors"
    (let [tag1 (scope/scope-tag)
          tag2 (scope/scope-tag)
          i1 (scope/ident tag1 0 'x)
          i2 (scope/ident tag1 1 'y)
          i3 (scope/ident tag2 0 'a)
          i4 (scope/ident tag2 1 'b)
          vec [i1 i2]
          reidentified (scope/reident vec {i1 i3, i2 i4})]
      (is (= [i3 i4] reidentified)))))

;;; ============================================================================
;;; Utility Function Tests
;;; ============================================================================

(deftest test-collect-tags
  (testing "Collects tags from single ident"
    (let [tag (scope/scope-tag)
          ident (scope/ident tag 0 'x)
          tags (scope/collect-tags ident)]
      (is (= #{tag} tags))))

  (testing "Collects tags from nested structure"
    (let [tag1 (scope/scope-tag)
          tag2 (scope/scope-tag)
          i1 (scope/ident tag1 0 'x)
          i2 (scope/ident tag2 1 'y)
          structure {:foo [i1 {:bar i2}]}
          tags (scope/collect-tags structure)]
      (is (= #{tag1 tag2} tags))))

  (testing "Returns empty set for non-scoped values"
    (is (empty? (scope/collect-tags 42)))
    (is (empty? (scope/collect-tags "hello")))
    (is (empty? (scope/collect-tags {:a 1 :b 2})))))

(deftest test-alpha-equivalent
  (testing "Same idents are alpha-equivalent"
    (let [tag (scope/scope-tag)
          i1 (scope/ident tag 0 'x)
          i2 (scope/ident tag 0 'x)]
      (is (scope/alpha-equivalent? i1 i2))))

  (testing "Idents with different tags but same lid/name are alpha-equivalent"
    (let [tag1 (scope/scope-tag)
          tag2 (scope/scope-tag)
          i1 (scope/ident tag1 0 'x)
          i2 (scope/ident tag2 0 'x)]
      (is (scope/alpha-equivalent? i1 i2))))

  (testing "Idents with different lids are not alpha-equivalent"
    (let [tag (scope/scope-tag)
          i1 (scope/ident tag 0 'x)
          i2 (scope/ident tag 1 'x)]
      (is (not (scope/alpha-equivalent? i1 i2)))))

  (testing "Idents with different names are not alpha-equivalent"
    (let [tag (scope/scope-tag)
          i1 (scope/ident tag 0 'x)
          i2 (scope/ident tag 0 'y)]
      (is (not (scope/alpha-equivalent? i1 i2)))))

  (testing "Works on nested structures"
    (let [tag1 (scope/scope-tag)
          tag2 (scope/scope-tag)
          i1 (scope/ident tag1 0 'x)
          i2 (scope/ident tag2 0 'x)
          struct1 [i1 {:foo i1}]
          struct2 [i2 {:foo i2}]]
      (is (scope/alpha-equivalent? struct1 struct2)))))

;;; ============================================================================
;;; Integration/Example Tests (Theory Morphism Simulation)
;;; ============================================================================

(deftest test-theory-morphism-simulation
  (testing "Simulating a theory morphism with scope hygiene"
    ;; Setup: Theory 1 has 'Ob' and 'Hom'
    (let [tag1 (scope/scope-tag)
          ctx1 (scope/scope-context tag1)
          [ctx1 ob1] (scope/bind ctx1 'Ob)
          [ctx1 hom1] (scope/bind ctx1 'Hom)

          ;; Theory 2 also has 'Ob' and 'Hom'
          tag2 (scope/scope-tag)
          ctx2 (scope/scope-context tag2)
          [ctx2 ob2] (scope/bind ctx2 'Ob)
          [ctx2 hom2] (scope/bind ctx2 'Hom)

          ;; A term in Theory 1 using its identifiers
          term1 [hom1 ob1 ob1]

          ;; Map Theory 1's identifiers to Theory 2's
          ident-map {ob1 ob2, hom1 hom2}

          ;; Apply morphism
          term2 (scope/reident term1 ident-map)]

      ;; Verify the mapping worked
      (is (= [hom2 ob2 ob2] term2))
      (is (= tag2 (-> term2 first :tag)))
      ;; After reident, terms have different scope tags but same structure
      (is (scope/alpha-equivalent? term1 term2)
          "Terms are alpha-equivalent despite different scope tags")
      ;; But they're not equal (different scope tags)
      (is (not= term1 term2)
          "Terms from different theories are not equal")))

  (testing "Demonstrating why scope tags prevent collisions"
    ;; Without scope tags, we'd have name collisions
    ;; With them, we can safely distinguish variables from different theories
    (let [tag1 (scope/scope-tag)
          tag2 (scope/scope-tag)

          ;; Same name 'x' in two different scopes
          x-in-theory1 (scope/ident tag1 0 'x)
          x-in-theory2 (scope/ident tag2 0 'x)]

      ;; They're different identifiers despite same name
      (is (not= x-in-theory1 x-in-theory2))

      ;; But they're alpha-equivalent
      (is (scope/alpha-equivalent? x-in-theory1 x-in-theory2))

      ;; Can use retag to "move" variables between theories
      (let [moved-x (scope/retag x-in-theory1 {tag1 tag2})]
        (is (= tag2 (:tag moved-x)))
        (is (= 'x (:name moved-x)))))))
