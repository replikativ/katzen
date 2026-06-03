(ns katzen.xref-test
  "Cross-ACSet reference = pullback over a shared identity AttrType."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.acset :as a]
            [katzen.xref :as xref]))

;; Two separate ACSets, two separate schemas, ONE shared AttrType `Identity`.
;; Code side: a Def has a `qname` identity. KB side: a Note is `about` a qname.

(def code-schema
  {:objects   [:Def]
   :homs      []
   :attr-types [:Identity :Source]
   :attrs     [{:name :qname  :dom :Def :codom :Identity}
               {:name :source :dom :Def :codom :Source}]})

(def kb-schema
  {:objects   [:Note]
   :homs      []
   :attr-types [:Identity :Text]
   :attrs     [{:name :about :dom :Note :codom :Identity}
               {:name :text  :dom :Note :codom :Text}]})

(defn- code-acset []
  (-> (a/vector-acset code-schema)
      (a/add-part-with :Def {:qname 'demo.core/a :source "(defn a [x] (* 2 x))"})
      (a/add-part-with :Def {:qname 'demo.core/b :source "(defn b [y] (+ y 1))"})
      (a/add-part-with :Def {:qname 'demo.core/c :source "(defn c [] :c)"})))

(defn- kb-acset []
  (-> (a/vector-acset kb-schema)
      (a/add-part-with :Note {:about 'demo.core/a :text "hot path"})
      (a/add-part-with :Note {:about 'demo.core/b :text "memoize me"})
      ;; a note about something that isn't in the code ACSet → dangling
      (a/add-part-with :Note {:about 'demo.core/gone :text "stale link"})))

(deftest xref-is-the-pullback-over-the-shared-identity
  (testing "every (Note, Def) pair whose identity coincides"
    (let [pairs (xref/xref (kb-acset) :about (code-acset) :qname)
          ids   (set (map :id pairs))]
      (is (= 2 (count pairs)) "a and b match; gone has no Def; c has no Note")
      (is (= '#{demo.core/a demo.core/b} ids))
      (doseq [{:keys [a b id]} pairs]
        (is (some? a)) (is (some? b)) (is (some? id))))))

(deftest xref-is-symmetric-in-the-cospan
  (testing "swapping the two sides yields the same identity links"
    (let [from-kb (set (map :id (xref/xref (kb-acset) :about (code-acset) :qname)))
          from-code (set (map :id (xref/xref (code-acset) :qname (kb-acset) :about)))]
      (is (= from-kb from-code '#{demo.core/a demo.core/b})))))

(deftest dangling-finds-unresolved-cross-references
  (testing "a KB note whose qname has no Def is a broken link (a query, not an FK)"
    (let [d (xref/dangling (kb-acset) :about (code-acset) :qname)]
      (is (= '[demo.core/gone] (map :id d))))
    (testing "a Def with no Note is NOT dangling from the code side either way"
      ;; c has no note, but c's qname is a fine identity; dangling is about
      ;; whether the *reference* resolves, computed from the referring side.
      (is (= '#{demo.core/c}
             (set (map :id (xref/dangling (code-acset) :qname (kb-acset) :about))))))))

(deftest xref-requires-a-shared-attr-type
  (testing "cross-referencing across different identity spaces is rejected"
    (is (thrown? clojure.lang.ExceptionInfo
                 ;; :source (a :Source attr) is not the identity space of :about
                 (xref/xref (code-acset) :source (kb-acset) :about)))))
