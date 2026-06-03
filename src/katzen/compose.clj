(ns katzen.compose
  "The unified front door for operadic composition.

   `oapply` composes a map of box-systems through a wiring diagram. In
   Catlab/AlgebraicDynamics this is a single `oapply` overloaded by
   multiple dispatch on (diagram type × box-algebra type); the actual
   composition *law* is bespoke per (operad, algebra) pair. katzen
   implements each law in its own namespace — `uwd.dynamics/oapply`
   (resource sharers), `dwd.dynamics/oapply-dwd` (machines),
   `cpg/oapply-cpg` (machines on circular port graphs) — and this
   `defmulti` is the single symbol over them, dispatching on the
   diagram's operad (recovered from its schema) and the boxes' algebra
   (recovered from the box-system record type). So a caller writes
   `(oapply d boxes)` regardless of operad; the suffix-naming
   (`oapply-dwd`, …) is no longer the caller's burden.

   The set-valued / scalar-valued UWD algebras (`uwd/oapply-relations`,
   `uwd.algebras/oapply-scalar`) and `petri/compose-petri` are *not*
   methods here: they carry algebra-specific extra arguments (a junction
   type cardinality, an eval/combine/unit, a port→species map), so they
   are a genuinely different function shape, not the same call at a
   different type. They keep their own names by design."
  (:require [katzen.acset :as a]
            [katzen.cpg :as cpg]
            [katzen.dwd.dynamics :as ddyn]
            [katzen.uwd.dynamics :as udyn])
  (:import [katzen.uwd.dynamics ContinuousResourceSharer]
           [katzen.dwd.dynamics Machine]))

(defn- operad-of
  "The operad a wiring diagram belongs to — the name of its ACSet schema
   (`SchUWD` / `SchDWD` / `SchCPG`)."
  [d]
  (:name (a/schema d)))

(defn- algebra-of
  "The algebra a box-system map carries — recovered from the record type
   of any box's system value."
  [box->sys]
  (when (empty? box->sys)
    (throw (ex-info "oapply: empty box map — cannot infer the algebra" {})))
  (let [v (val (first box->sys))]
    (condp instance? v
      ContinuousResourceSharer :resource-sharer
      Machine                  :machine
      (throw (ex-info "oapply: unrecognized box-system algebra"
                      {:value v :type (type v)})))))

(defmulti oapply
  "Compose `box->sys` through the wiring diagram `d`, dispatching on
   `[operad algebra]`. The single entry point over the per-operad laws.

     (oapply uwd {b1 crs1  b2 crs2})       ; → a ContinuousResourceSharer
     (oapply dwd {b1 mach1 b2 mach2})      ; → a Machine
     (oapply cpg {b1 mach1 b2 mach2})      ; → a Machine"
  (fn [d box->sys] [(operad-of d) (algebra-of box->sys)]))

(defmethod oapply ['SchUWD :resource-sharer] [d boxes] (udyn/oapply d boxes))
(defmethod oapply ['SchDWD :machine]         [d boxes] (ddyn/oapply-dwd d boxes))
(defmethod oapply ['SchCPG :machine]         [d boxes] (cpg/oapply-cpg d boxes))

(defmethod oapply :default [d box->sys]
  (throw (ex-info "oapply: no composition law for this operad × algebra"
                  {:operad  (operad-of d)
                   :algebra (try (algebra-of box->sys) (catch Exception _ :unknown))})))
