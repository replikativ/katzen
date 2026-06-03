(ns katzen.ansatz-bridge
  "Live probe: exercise the katzen.ansatz.export API end-to-end against
   a Mathlib-loaded ansatz environment.

   This file replaces the original a/structure-based probe (which
   silently degraded dependent sorts). The new path goes through
   katzen.ansatz.export, which emits ConstantInfo via the ansatz kernel
   and verifies theories AND instances in Lean 4's CIC.

   Run from a REPL started with: clojure -M:dev:ansatz:repl -m nrepl.cmdline
   Requires: pre-built ansatz store at /var/tmp/ansatz-mathlib.

       (require 'katzen.ansatz-bridge :reload)
       (katzen.ansatz-bridge/probe-all!)

   Status (2026-06-02): all 5 stdlib theories verified end-to-end,
   including ThCategory with dependent sorts and ThSymMonCat with
   inheritance from ThCategory."
  (:require [ansatz.core :as a]
            [katzen.ansatz.export :as ax]
            [katzen.stdlib.core :as std]))

(defn- safe-check [label f]
  (try
    (let [r (f)]
      (println (format "  ✓ %-35s %s" label r)))
    (catch Exception e
      (println (format "  ✗ %-35s %s" label (.getMessage e))))))

(defn probe-theories!
  "Run check-theory! against every stdlib theory."
  []
  (println "\n=== check-theory! over stdlib ===")
  (safe-check "ThGraph"                       #(ax/check-theory! std/ThGraph))
  (safe-check "ThMonoid"                      #(ax/check-theory! std/ThMonoid))
  (safe-check "ThCategory"                    #(ax/check-theory! std/ThCategory))
  (safe-check "ThGroup (using ThMonoid)"      #(ax/check-theory! std/ThGroup))
  (safe-check "ThSymmetricMonoidalCategory"   #(ax/check-theory! std/ThSymmetricMonoidalCategory)))

(defn probe-instances!
  "Run check-instance! across a few simple sort bindings."
  []
  (println "\n=== check-instance! shape checks ===")
  (safe-check "ThMonoid {El=Nat}"
              #(ax/check-instance! std/ThMonoid '{El Nat}))
  (safe-check "ThMonoid {El=Bool}"
              #(ax/check-instance! std/ThMonoid '{El Bool}))
  (safe-check "ThCategory {Ob=Nat, Hom=λ_ _ ⇒ Nat}"
              #(ax/check-instance! std/ThCategory
                                   '{Ob Nat
                                     Hom (lam [a Nat, b Nat] Nat)}))
  (safe-check "ThCategory missing Hom (should err)"
              #(ax/check-instance! std/ThCategory '{Ob Nat})))

(defn probe-all! []
  (probe-theories!)
  (probe-instances!)
  (println "\ndone."))

(comment
  ;; From the REPL:
  (a/init! "/var/tmp/ansatz-mathlib" "mathlib")
  (probe-all!))
