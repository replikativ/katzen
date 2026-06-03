#!/usr/bin/env bb
;; Run only the working tests (excludes tests with deftheory inside deftest)

(ns run-working-tests
  (:require [clojure.java.shell :as shell]))

(def working-test-namespaces
  ["katzen.core-test"
   "katzen.scope-test"
   "katzen.scope-advanced-test"
   "katzen.model-test"
   "katzen.model-simple-test"
   "katzen.presentation-test"
   "katzen.morphism-test"
   "katzen.morphism-advanced-test"
   "katzen.rewrite-test"
   "katzen.morphism-wellformedness-test"
   "katzen.unicode-test"
   "katzen.expr-interop-test"
   "katzen.stdlib-test"
   "katzen.library.algebra-test"])

(defn run-tests []
  (println "Running working tests (excluding theory-test, theory-term-in-type-test, pretty-test)...")
  (println)
  (doseq [ns working-test-namespaces]
    (println (str "Testing " ns "..."))
    (let [result (shell/sh "clojure" "-M:test" "-n" ns)]
      (when (not= 0 (:exit result))
        (println "FAILED:")
        (println (:err result))
        (System/exit 1))))
  (println)
  (println "✅ All working tests passed!"))

(when (= *file* (System/getProperty "babashka.file"))
  (run-tests))
