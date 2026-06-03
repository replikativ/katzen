(ns notebooks.comparison-with-catlab
  "Comparison of katzen with the Catlab.jl / GATlab.jl stack on
   representative scenarios. Runnable as a script:

     clojure -M:dev:raster:ansatz -m notebooks.comparison-with-catlab

   Sections gracefully skip if their optional dependency (raster,
   ansatz) is absent. The base section (instance axiom checking +
   symbolic normalization) needs no optional dependencies.

   For each scenario the script prints the katzen code, runs it,
   and notes the specific Julia file/line where Catlab or GATlab
   does the same thing (or explicitly punts). The 'verdict' at the
   end of each section is the operational comparison: do we MATCH,
   EXCEED, or LAG the Julia stack?

   Source for the Julia citations: the codebase survey in
   `doc/design-analysis/` and the live repos at
   `/home/christian-weilbach/Development/{Catlab,GATlab}.jl/`."
  (:require [katzen.acset :as a]
            [katzen.acset.check :as check]
            [katzen.acset.graphs :as gg]
            [katzen.acset.morphism :as am]
            [katzen.acset.normalize :as norm]
            [katzen.acset.homomorphism :as hom]))

;; ============================================================================
;; Pretty section headers
;; ============================================================================

(defn- section [title]
  (println)
  (println (apply str (repeat 78 \=)))
  (println title)
  (println (apply str (repeat 78 \=))))

(defn- subsec [title]
  (println)
  (println (str "── " title " ──")))

(defn- skip [reason]
  (println (str "  [SKIPPED] " reason)))

(defn- verdict [where line]
  (println)
  (println (str "▸ " where ":"))
  (println (str "  " line)))

;; ============================================================================
;; Section 1 — Schema axioms: enforcement vs declaration
;; ============================================================================

(defn section-1-axiom-enforcement []
  (section "1. Schema axioms — enforcement (us) vs declaration (Julia)")

  (subsec "1a. Define a symmetric-graph schema with the involution axiom")
  (let [SchSym (assoc gg/SchSymmetricGraph
                      :name 'SchSymInvolution
                      :axioms [{:name 'inv-involution
                                :ctx [{:name 'e :type :E}]
                                :lhs '(inv (inv e))
                                :rhs 'e}])
        good (let [g (a/vector-acset SchSym)
                   [g _]      (a/add-parts g :V 2)
                   [g [e1 e2]] (a/add-parts g :E 2)]
               (-> g
                   (a/set-subpart :src e1 1) (a/set-subpart :tgt e1 2)
                   (a/set-subpart :src e2 2) (a/set-subpart :tgt e2 1)
                   (a/set-subpart :inv e1 e2) (a/set-subpart :inv e2 e1)))
        bad (let [g good
                  [g [e3]] (a/add-parts g :E 1)
                  g (-> g (a/set-subpart :src e3 1)
                          (a/set-subpart :tgt e3 1)
                          (a/set-subpart :inv e3 1))]   ; inv e3 = e1, breaking involution
              g)]

    (println)
    (println "good instance:" (count (a/parts good :E)) "edges, every inv pair consistent")
    (println "check-axioms!: " (check/check-axioms good))

    (println)
    (println "bad instance: third edge with inv pointing wrong")
    (println "check-axioms!: " (check/check-axioms bad))

    (verdict "Julia equivalent (Catlab)"
             "src/graphs/BasicGraphs.jl:300-311 — add_edges! manually maintains
  the involution invariant. There is no `check_axioms` for ACSets. The
  schema's stored equations are documentation; runtime enforcement is
  on the user."))

  (verdict "Verdict" "EXCEED — we enforce at the data boundary; Julia leaves it to the user."))

;; ============================================================================
;; Section 2 — Symbolic normalization
;; ============================================================================

(defn section-2-normalization []
  (section "2. Symbolic normalization — directed rewrites")

  (subsec "2a. Schema-axiom normalization (SchSym + involution)")

  (let [SchSym (assoc gg/SchSymmetricGraph
                      :name 'SchSym
                      :axioms [{:name 'inv-involution
                                :ctx [{:name 'e :type :E}]
                                :lhs '(inv (inv e))
                                :rhs 'e}])]
    (doseq [t ['(inv (inv x))
                '(inv (inv (inv x)))
                '(inv (inv (inv (inv x))))
                '(src (inv (inv e)))]]
      (println " " t "  →  " (norm/normalize SchSym t))))

  (subsec "2b. Monoid normalization with associativity hint")

  (let [SchM {:name 'SchMonoid :objects [:El] :homs []
              :axioms [{:name 'assoc
                        :ctx [{:name 'x :type :El} {:name 'y :type :El} {:name 'z :type :El}]
                        :lhs '(mul (mul x y) z)
                        :rhs '(mul x (mul y z))
                        :canonical :lhs}    ; left-associative is canonical
                       {:name 'unit-left :ctx [{:name 'x :type :El}]
                        :lhs '(mul u x) :rhs 'x}
                       {:name 'unit-right :ctx [{:name 'x :type :El}]
                        :lhs '(mul x u) :rhs 'x}]}]
    (doseq [t ['(mul a (mul b c))
                '(mul a (mul b (mul c d)))
                '(mul u (mul a (mul u b)))
                '(mul (mul a u) (mul b u))]]
      (println " " t "  →  " (norm/normalize SchM t))))

  (verdict "Julia equivalent (Catlab/GATlab)"
           "src/models/GATExprUtils.jl:30 `associate_unit`, :82 `involute`,
  :92 `normalize_zero`. Wired into smart constructors of every
  `@symbolic_model`: `compose(f::Hom, g::Hom) = associate_unit(...)`.
  104 LOC of directed rewrites, no e-graph. The aspirational
  e-graph layer (catlab_differences.md:23) doesn't exist in code.")

  (verdict "Verdict" "MATCH — same directed-rewrite approach, same coverage."))

;; ============================================================================
;; Section 3 — Verified morphisms (ansatz)
;; ============================================================================

(defn section-3-verified-morphisms []
  (section "3. Verified morphisms — Lean-kernel proofs (us) vs structural-only checks (Julia)")

  (subsec "3a. Bridge an axiomized schema to a katzen GAT and verify a morphism")

  (let [bridge-available? (try (require 'katzen.acset.theory-bridge) true
                               (catch Throwable _ nil))
        ansatz-ready? (and bridge-available?
                           (.exists (java.io.File. "/var/tmp/ansatz-mathlib")))]
    (cond
      (not bridge-available?)
      (skip "theory-bridge namespace not available")

      (not ansatz-ready?)
      (skip "/var/tmp/ansatz-mathlib not found — set up the Mathlib store
       per doc/design-analysis (or skip this section).")

      :else
      (let [_ ((requiring-resolve 'ansatz.core/init!) "/var/tmp/ansatz-mathlib" "mathlib")
            mig (requiring-resolve 'katzen.acset.migration/schema-morphism)
            bridge (find-ns 'katzen.acset.theory-bridge)
            verify! (ns-resolve bridge 'verify-schema-morphism!)
            SchSymAx (assoc gg/SchSymmetricGraph
                            :name 'SchSymAx
                            :axioms [{:name 'inv-involution
                                      :ctx [{:name 'e :type :E}]
                                      :lhs '(inv (inv e))
                                      :rhs 'e}])
            IdSymAx (mig 'IdSymAx SchSymAx SchSymAx
                         {:V :V :E :E}
                         {:src [:src] :tgt [:tgt] :inv [:inv]})]
        (println "verify-schema-morphism! IdSymAx ..." )
        (verify! IdSymAx)
        (println "→ verified through ansatz: schema bridged to a GAT, theory")
        (println "  morphism's axiom obligations discharged through the Lean kernel.")

        (verdict "Julia equivalent (GATlab)"
                 "src/syntax/TheoryMaps.jl:256 — *\"axioms are not mapped to
  proofs. TODO: check that it is well-formed, axioms are
  preserved.\"* The TODO is open in the current Julia release."))))

  (verdict "Verdict" "EXCEED — we ship the axiom-preservation check via Lean; Julia has a TODO."))

;; ============================================================================
;; Section 4 — ACSet morphisms and naturality
;; ============================================================================

(defn section-4-naturality []
  (section "4. ACSet morphisms — naturality check + the homomorphism lift")

  (let [tri (let [[g _] (a/add-vertices (a/graph) 3)
                  [g _] (a/add-edge g 1 2)
                  [g _] (a/add-edge g 2 3)
                  [g _] (a/add-edge g 3 1)]
              g)
        probe (let [[g _] (a/add-vertices (a/graph) 2)
                    [g _] (a/add-edge g 1 2)]
                g)]
    (subsec "4a. Identity is natural by construction")
    (println "natural? (identity-morphism triangle):"
             (am/natural? (am/identity-morphism tri)))

    (subsec "4b. Every homomorphism search result lifts to a natural morphism")
    (let [homs (hom/homomorphisms probe tri)
          morphs (map #(am/from-flat-components probe tri %) homs)]
      (println "found" (count homs) "homomorphisms; all natural?" (every? am/natural? morphs))))

  (verdict "Julia equivalent (Catlab)"
           "src/categorical_algebra/pointwise/csets/CSets.jl:208 `is_natural`.
  Same predicate, same role. Catlab routinely calls it from
  homomorphism search to prune non-natural assignments; we expose it
  as a separate check on results — equivalent guarantee, slightly
  different surface.")

  (verdict "Verdict" "MATCH — same structural law, same coverage."))

;; ============================================================================
;; Section 5 — Numerical compilation (raster bridge)
;; ============================================================================

(defn section-5-numerical-compile []
  (section "5. Categorical → numerical compilation — typed RHS for ODE solvers")

  (cond
    (not (try (require 'raster.ode) true (catch Throwable _ nil)))
    (skip "raster not available; rerun with `clojure -M:dev:raster ...`")

    :else
    (do
      (require 'katzen.petri 'katzen.compile.core)
      (let [compile-rhs (requiring-resolve 'katzen.compile.core/compile-rhs)
            petri-dyn   (requiring-resolve 'katzen.petri/petri-dynamics)
            solve       (requiring-resolve 'raster.ode/solve)
            tsit5       (requiring-resolve 'raster.ode/tsit5)
            ode-problem (requiring-resolve 'raster.ode/ode-problem)
            add-sp      (requiring-resolve 'katzen.petri/add-species)
            add-tr      (requiring-resolve 'katzen.petri/add-transition)
            add-in      (requiring-resolve 'katzen.petri/add-input)
            add-out     (requiring-resolve 'katzen.petri/add-output)
            petri       (requiring-resolve 'katzen.petri/petri)

            n       (petri)
            [n s]   (add-sp n)
            [n i]   (add-sp n)
            [n r]   (add-sp n)
            [n inf] (add-tr n)
            [n rec] (add-tr n)
            [n _]   (add-in n s inf)
            [n _]   (add-in n i inf)
            [n _]   (add-out n i inf)
            [n _]   (add-out n i inf)
            [n _]   (add-in n i rec)
            [n _]   (add-out n r rec)
            rhs (compile-rhs (petri-dyn n {inf 0.0003 rec 0.1}))
          u0 (double-array [999.0 1.0 0.0])
          sol (solve (tsit5) (ode-problem rhs u0 0.0 100.0) 1.0)
          final (last (:us sol))]
      (println "SIR compiled to raster typed RHS; final state at t=100:")
      (println " " (vec final))
      (println "  → categorical ACSet → raster.core/ftm → JIT'd typed function")
      (verdict "Julia equivalent (AlgebraicPetri)"
               "src/AlgebraicPetri.jl:244 `vectorfield`: returns a plain Julia
  closure over precomputed multiplicities. Slower per-stage (closure
  + symbolic indexing) but works. `vectorfield_expr` (:336) builds an
  Expr and uses GeneralizedGenerated.mk_function to JIT — the same
  spirit as our raster.core/ftm path, different mechanism.")

      (verdict "Verdict" "MATCH (architecturally) — both ship a closure path and a JIT-compiled path.")))))

;; ============================================================================
;; Summary
;; ============================================================================

(defn summary []
  (section "Summary")
  (println)
  (println "  Where we MATCH the Julia stack:")
  (println "    • Symbolic normalization (Layer 1+2 ≈ GATExprUtils.jl)")
  (println "    • ACSet morphism naturality (am/natural? ≈ Catlab's is_natural)")
  (println "    • Numerical compile to typed RHS (compile-rhs ≈ vectorfield_expr)")
  (println)
  (println "  Where we EXCEED the Julia stack:")
  (println "    • Instance-level axiom enforcement (check-axioms!) — Catlab punts")
  (println "    • Theory morphism axiom preservation via Lean kernel (theory-bridge)")
  (println "    • Functor migration of morphisms (migrate-morphism) — Catlab doesn't expose")
  (println)
  (println "  Where we LAG the Julia stack:")
  (println "    • E-graph normalization for non-orientable equational reasoning")
  (println "      (Julia explicitly punts here too — `catlab_differences.md:23` is aspirational)")
  (println "    • Chase algorithm for closing partial instances (Catlab has it; we don't)")
  (println "    • Multi-step path support in the theory bridge (single-step only for now)")
  (println)
  (println "  Where neither stack covers (open territory):")
  (println "    • E-graph-backed equiv? routed through ansatz/grind")
  (println "    • oapply coherence verification")
  (println "    • Compile-output static lint")
  (println))

;; ============================================================================
;; Entry point
;; ============================================================================

(defn -main [& _]
  (println "katzen ↔ Catlab.jl / GATlab.jl — feature comparison")
  (section-1-axiom-enforcement)
  (section-2-normalization)
  (section-3-verified-morphisms)
  (section-4-naturality)
  (section-5-numerical-compile)
  (summary))
