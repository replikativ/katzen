(ns katzen.scope-advanced-test
  "Tests for advanced multi-level scoping features."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.scope :as scope]))

;;; ============================================================================
;;; ScopeList Tests
;;; ============================================================================

(deftest test-scope-list-creation
  (testing "Can create a ScopeList from multiple scopes"
    (let [scope1 (scope/scope-context (scope/scope-tag))
          scope2 (scope/scope-context (scope/scope-tag))
          slist (scope/scope-list [scope1 scope2])]
      (is (scope/scope-list? slist))
      (is (= 2 (scope/nscopes slist)))))

  (testing "ScopeList rejects duplicate tags"
    (let [tag (scope/scope-tag)
          scope1 (scope/scope-context tag)
          scope2 (scope/scope-context tag)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"duplicate tags"
           (scope/scope-list [scope1 scope2]))))))

(deftest test-scope-list-empty
  (testing "Empty ScopeList has zero scopes"
    (let [slist (scope/scope-list [])]
      (is (= 0 (scope/nscopes slist))))))

(deftest test-nscopes
  (testing "nscopes counts scopes correctly"
    (let [ctx1 (scope/scope-context)
          scope1 (scope/scope-context)
          scope2 (scope/scope-context)
          slist (scope/scope-list [scope1 scope2])]
      (is (= 1 (scope/nscopes ctx1)))
      (is (= 2 (scope/nscopes slist))))))

;;; ============================================================================
;;; getscope Tests
;;; ============================================================================

(deftest test-getscope-single
  (testing "getscope on ScopeContext"
    (let [ctx (scope/scope-context)]
      (is (= ctx (scope/getscope ctx 1)))
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"out of bounds"
           (scope/getscope ctx 2))))))

(deftest test-getscope-list
  (testing "getscope on ScopeList"
    (let [scope1 (scope/scope-context)
          scope2 (scope/scope-context)
          slist (scope/scope-list [scope1 scope2])]
      (is (= scope1 (scope/getscope slist 1)))
      (is (= scope2 (scope/getscope slist 2)))
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"out of bounds"
           (scope/getscope slist 3))))))

;;; ============================================================================
;;; getlevel Tests
;;; ============================================================================

(deftest test-getlevel-by-tag
  (testing "getlevel finds tag level in ScopeList"
    (let [tag1 (scope/scope-tag)
          tag2 (scope/scope-tag)
          scope1 (scope/scope-context tag1)
          scope2 (scope/scope-context tag2)
          slist (scope/scope-list [scope1 scope2])]
      (is (= 1 (scope/getlevel slist tag1)))
      (is (= 2 (scope/getlevel slist tag2)))))

  (testing "getlevel throws on missing tag"
    (let [scope1 (scope/scope-context)
          slist (scope/scope-list [scope1])
          missing-tag (scope/scope-tag)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Tag not found"
           (scope/getlevel slist missing-tag))))))

(deftest test-getlevel-by-name
  (testing "getlevel finds name level in ScopeList (most recent)"
    (let [scope1 (scope/scope-context)
          [scope1 x1] (scope/bind scope1 'x)
          scope2 (scope/scope-context)
          [scope2 x2] (scope/bind scope2 'x)
          [scope2 y2] (scope/bind scope2 'y)
          slist (scope/scope-list [scope1 scope2])]
      ;; x appears in both, should find most recent (level 1)
      (is (= 1 (scope/getlevel slist 'x)))
      ;; y only appears in level 2
      (is (= 2 (scope/getlevel slist 'y)))))

  (testing "getlevel throws on missing name"
    (let [scope1 (scope/scope-context)
          slist (scope/scope-list [scope1])]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Name not found"
           (scope/getlevel slist 'missing))))))

;;; ============================================================================
;;; hastag? and hasname? Tests
;;; ============================================================================

(deftest test-hastag
  (testing "hastag? returns true for existing tag"
    (let [tag (scope/scope-tag)
          scope1 (scope/scope-context tag)
          slist (scope/scope-list [scope1])]
      (is (scope/hastag? slist tag))))

  (testing "hastag? returns false for missing tag"
    (let [scope1 (scope/scope-context)
          slist (scope/scope-list [scope1])
          missing-tag (scope/scope-tag)]
      (is (not (scope/hastag? slist missing-tag))))))

(deftest test-hasname
  (testing "hasname? returns true for existing name"
    (let [scope1 (scope/scope-context)
          [scope1 x] (scope/bind scope1 'x)
          slist (scope/scope-list [scope1])]
      (is (scope/hasname? slist 'x))))

  (testing "hasname? returns false for missing name"
    (let [scope1 (scope/scope-context)
          slist (scope/scope-list [scope1])]
      (is (not (scope/hasname? slist 'missing))))))

;;; ============================================================================
;;; lookup-multi Tests
;;; ============================================================================

(deftest test-lookup-multi
  (testing "lookup-multi finds name in most recent scope"
    (let [scope1 (scope/scope-context)
          [scope1 x1] (scope/bind scope1 'x)
          scope2 (scope/scope-context)
          [scope2 x2] (scope/bind scope2 'x)
          slist (scope/scope-list [scope1 scope2])]
      ;; Should find x from scope1 (most recent)
      (is (= x1 (scope/lookup-multi slist 'x)))))

  (testing "lookup-multi finds name in older scope if not in recent"
    (let [scope1 (scope/scope-context)
          [scope1 x] (scope/bind scope1 'x)
          scope2 (scope/scope-context)
          [scope2 y] (scope/bind scope2 'y)
          slist (scope/scope-list [scope1 scope2])]
      (is (= x (scope/lookup-multi slist 'x)))
      (is (= y (scope/lookup-multi slist 'y)))))

  (testing "lookup-multi returns nil for missing name"
    (let [scope1 (scope/scope-context)
          slist (scope/scope-list [scope1])]
      (is (nil? (scope/lookup-multi slist 'missing))))))

;;; ============================================================================
;;; AppendContext Tests
;;; ============================================================================

(deftest test-append-context-creation
  (testing "Can create AppendContext"
    (let [scope1 (scope/scope-context)
          scope2 (scope/scope-context)
          slist (scope/scope-list [scope1])
          appended (scope/append-context slist scope2)]
      (is (= 2 (scope/nscopes appended)))))

  (testing "AppendContext rejects duplicate tag"
    (let [tag (scope/scope-tag)
          scope1 (scope/scope-context tag)
          scope2 (scope/scope-context tag)
          slist (scope/scope-list [scope1])]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"duplicate tag"
           (scope/append-context slist scope2))))))

(deftest test-append-context-getscope
  (testing "getscope works on AppendContext"
    (let [scope1 (scope/scope-context)
          scope2 (scope/scope-context)
          slist (scope/scope-list [scope1])
          appended (scope/append-context slist scope2)]
      (is (= scope1 (scope/getscope appended 1)))
      (is (= scope2 (scope/getscope appended 2))))))

(deftest test-append-context-getlevel
  (testing "getlevel works on AppendContext"
    (let [tag1 (scope/scope-tag)
          tag2 (scope/scope-tag)
          scope1 (scope/scope-context tag1)
          scope2 (scope/scope-context tag2)
          slist (scope/scope-list [scope1])
          appended (scope/append-context slist scope2)]
      (is (= 1 (scope/getlevel appended tag1)))
      (is (= 2 (scope/getlevel appended tag2))))))

(deftest test-append-context-lookup
  (testing "lookup-multi works on AppendContext"
    (let [scope1 (scope/scope-context)
          [scope1 x] (scope/bind scope1 'x)
          scope2 (scope/scope-context)
          [scope2 y] (scope/bind scope2 'y)
          slist (scope/scope-list [scope1])
          appended (scope/append-context slist scope2)]
      (is (= y (scope/lookup-multi appended 'y)))
      (is (= x (scope/lookup-multi appended 'x))))))

;;; ============================================================================
;;; getidents Tests
;;; ============================================================================

(deftest test-getidents
  (testing "getidents returns all idents from single scope"
    (let [scope1 (scope/scope-context)
          [scope1 x] (scope/bind scope1 'x)
          [scope1 y] (scope/bind scope1 'y)
          idents (scope/getidents scope1)]
      (is (= 2 (count idents)))
      (is (some #{x} idents))
      (is (some #{y} idents))))

  (testing "getidents returns all idents from ScopeList"
    (let [scope1 (scope/scope-context)
          [scope1 x] (scope/bind scope1 'x)
          scope2 (scope/scope-context)
          [scope2 y] (scope/bind scope2 'y)
          slist (scope/scope-list [scope1 scope2])
          idents (scope/getidents slist)]
      (is (= 2 (count idents)))
      (is (some #{x} idents))
      (is (some #{y} idents))))

  (testing "getidents returns all idents from AppendContext"
    (let [scope1 (scope/scope-context)
          [scope1 x] (scope/bind scope1 'x)
          scope2 (scope/scope-context)
          [scope2 y] (scope/bind scope2 'y)
          slist (scope/scope-list [scope1])
          appended (scope/append-context slist scope2)
          idents (scope/getidents appended)]
      (is (= 2 (count idents)))
      (is (some #{x} idents))
      (is (some #{y} idents)))))

;;; ============================================================================
;;; EmptyContext Tests
;;; ============================================================================

(deftest test-empty-context
  (testing "Can create EmptyContext"
    (let [empty (scope/empty-context)]
      (is (scope/empty-context? empty))
      (is (= 0 (scope/nscopes empty)))))

  (testing "hastag? returns false on EmptyContext"
    (let [empty (scope/empty-context)
          tag (scope/scope-tag)]
      (is (not (scope/hastag? empty tag)))))

  (testing "hasname? returns false on EmptyContext"
    (let [empty (scope/empty-context)]
      (is (not (scope/hasname? empty 'x)))))

  (testing "getscope throws on EmptyContext"
    (let [empty (scope/empty-context)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"out of bounds|Cannot get scope"
           (scope/getscope empty 1)))))

  (testing "getlevel throws on EmptyContext"
    (let [empty (scope/empty-context)
          tag (scope/scope-tag)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"not found|Cannot query"
           (scope/getlevel empty tag)))
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"not found|Cannot query"
           (scope/getlevel empty 'x))))))
