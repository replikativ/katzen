(ns katzen.ansatz.stdlib-verified
  "Verified-on-load companion to katzen.stdlib.core.

   Requiring this namespace runs katzen.ansatz.export/check-theory!
   against every stdlib theory and throws if any fails the CIC check.
   This is the opt-in pattern for theories that should be Lean-kernel
   verified by construction.

   Prerequisite: callers must have run

     (require '[ansatz.core :as a])
     (a/init! \"/var/tmp/ansatz-mathlib\" \"mathlib\")

   before loading this ns, so the kernel environment is available.

   The verification is non-destructive — the ansatz global env is
   captured and restored around each check, so loading this ns does
   not pollute the session with auxiliary inductive declarations."
  (:require [katzen.ansatz.export :as ax]
            [katzen.stdlib.core :as std]))

(def verified-theories
  "Pairs of (katzen GAT, verification result) for every stdlib theory.

   Computed at namespace load time; each entry is :ok or the ex-info
   thrown by check-theory! is propagated, which aborts the load. The
   var lets downstream code introspect what was checked."
  [['ThGraph                     (ax/check-theory! std/ThGraph)]
   ['ThMonoid                    (ax/check-theory! std/ThMonoid)]
   ['ThCategory                  (ax/check-theory! std/ThCategory)]
   ['ThSchema                    (ax/check-theory! std/ThSchema)]
   ['ThGroup                     (ax/check-theory! std/ThGroup)]
   ['ThSymmetricMonoidalCategory (ax/check-theory! std/ThSymmetricMonoidalCategory)]])
