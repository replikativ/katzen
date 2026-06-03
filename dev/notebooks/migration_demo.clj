(ns notebooks.migration-demo
  "End-to-end demo: schema-driven data migration with type-safe forgetful
   migrations.

   Run as a script:
     clojure -M:dev -m notebooks.migration-demo
   That writes commentary + .dot/.svg pairs under dev/notebooks/output/.

   Or load in a REPL and explore step by step. Each `step` form prints a
   short narration; you can inspect intermediate ACSets between steps.

   This demo intentionally avoids the verification layer (ansatz) and the
   datahike persistence backend so it runs against the zero-dep core. The
   verified-migration story extends this demo by adding a check-morphism!
   call before each migrate, and the persistent-store story is the same
   demo with datahike-backed ACSets — both are short followups, not
   re-architectures."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [katzen.acset :as a]
            [katzen.acset.graphs :as gg]
            [katzen.acset.migration :as m]
            [katzen.acset.viz :as viz]))

(def out-dir "dev/notebooks/output")

(defn- write-dot
  "Write a dot source string to <name>.dot under out-dir. If `dot` is on
   the PATH, also render <name>.svg next to it."
  [filename dot-src]
  (.mkdirs (io/file out-dir))
  (let [dot-path (str out-dir "/" filename ".dot")
        svg-path (str out-dir "/" filename ".svg")]
    (spit dot-path dot-src)
    (when-let [dot-bin (try (-> (shell/sh "which" "dot") :out str/trim not-empty)
                            (catch Exception _ nil))]
      (let [r (shell/sh dot-bin "-Tsvg" "-o" svg-path dot-path)]
        (when-not (zero? (:exit r))
          (println "  (graphviz rendering failed:" (:err r) ")"))))
    (println "  wrote" dot-path)))

(defn- section [s]
  (println)
  (println (apply str (repeat 76 \=)))
  (println s)
  (println (apply str (repeat 76 \=))))

;; ============================================================================
;; Scenario: a small road network with distances
;; ============================================================================

(defn road-network
  "A weighted directed graph: 4 cities, 6 one-way connections."
  []
  (let [[g _] (a/add-parts (gg/weighted-graph) :V 4)
        ;; 1 = North, 2 = East, 3 = South, 4 = West
        [g _] (gg/add-weighted-edge g 1 2 120.0)   ; N → E, 120 km
        [g _] (gg/add-weighted-edge g 2 3 90.0)    ; E → S
        [g _] (gg/add-weighted-edge g 3 4 110.0)   ; S → W
        [g _] (gg/add-weighted-edge g 4 1 150.0)   ; W → N
        [g _] (gg/add-weighted-edge g 1 3 175.0)   ; N → S diagonal
        [g _] (gg/add-weighted-edge g 2 4 200.0)]  ; E → W diagonal
    g))

(def city-names
  "Vertex-id → human-readable city name for the road network."
  {1 "North" 2 "East" 3 "South" 4 "West"})

(defn- vertex-label [v]
  (get city-names v (str "v" v)))

;; ============================================================================
;; The walk-through
;; ============================================================================

(defn -main [& _]
  (section "Schema-driven data migration — katzen demo")
  (println
   (str "We start with a weighted directed graph representing four cities and\n"
        "six one-way roads between them. We then perform two contravariant\n"
        "(Δ) migrations — forgetful operations that move the data onto strictly\n"
        "simpler schemas — and visualize each stage with Graphviz."))

  (section "Step 1 — Define the data")
  (println "Schema: SchWeightedGraph.")
  (println "Instance: 4 cities, 6 roads, each road has a distance in km.")
  (let [network (road-network)]

    (println "\nSource-schema diagram:")
    (write-dot "01-sch-weighted"
               (viz/schema->dot gg/SchWeightedGraph))

    (println "\nSource instance:")
    (write-dot "02-network-weighted"
               (viz/graph->dot network
                               {:vertex-label vertex-label
                                :edge-label   viz/weight-edge-label}))

    (section "Step 2 — Forget the weights")
    (println
     (str "We want to do purely structural queries — connectivity, reachability,\n"
          "is-there-a-cycle — that don't depend on distances. The right tool is\n"
          "the schema inclusion SchGraph ↪ SchWeightedGraph and its induced\n"
          "Δ-migration ForgetWeight. After migration we have a SchGraph instance\n"
          "with the same topology but no weight column."))

    (let [topology (m/migrate gg/ForgetWeight network)]
      (write-dot "03-sch-plain"
                 (viz/schema->dot a/SchGraph))
      (write-dot "04-network-topology"
                 (viz/graph->dot topology {:vertex-label vertex-label}))
      (println "\nNow:")
      (println "  vertices:" (count (a/vertices topology)))
      (println "  edges:   " (count (a/edges topology)))
      (println "  (no Weight column — gone)")

      (section "Step 3 — Reverse every edge")
      (println
       (str "If we want to ask 'which cities can reach me' rather than 'which can\n"
            "I reach', we run the OpGraph migration. Conceptually F: SchGraph →\n"
            "SchGraph maps src to tgt and vice versa; Δ_F flips every edge."))

      (let [reversed (m/migrate m/OpGraph topology)]
        (write-dot "05-network-reversed"
                   (viz/graph->dot reversed {:vertex-label vertex-label}))
        (println "\nReversed edges:")
        (doseq [e (a/edges reversed)]
          (println " "
                   (vertex-label (a/src reversed e))
                   "→"
                   (vertex-label (a/tgt reversed e))))

        (section "Step 4 — Compose: weighted → topology → reversed")
        (println
         (str "The two migrations chain functorially: applying ForgetWeight then\n"
              "OpGraph to the road network is the same as applying their\n"
              "composition (Δ_F ∘ Δ_G = Δ_{G∘F}). At the schema-morphism level\n"
              "this composition is a single inclusion you could pre-build; here\n"
              "we just chain the two migrate calls and check shape parity."))

        (let [direct (-> network
                         (->> (m/migrate gg/ForgetWeight))
                         (->> (m/migrate m/OpGraph)))]
          (write-dot "06-network-forget-then-reverse"
                     (viz/graph->dot direct {:vertex-label vertex-label}))
          (println "\nFinal edge count: " (count (a/edges direct)))
          (println "Matches manual chain? "
                   (= (a/ne direct) (a/ne reversed))))

        (section "Done")
        (println
         (str "Outputs under " out-dir ":\n"
              "  01-sch-weighted        — schema of weighted graphs\n"
              "  02-network-weighted    — the 4-city road network with km\n"
              "  03-sch-plain           — schema of plain digraphs\n"
              "  04-network-topology    — after ForgetWeight\n"
              "  05-network-reversed    — after OpGraph on the topology\n"
              "  06-network-forget-then-reverse — composition\n\n"
              "Open the .svg files in a browser to inspect.\n\n"
              "What's next:\n"
              "  - verification: check-morphism! / check-instance! certify schema\n"
              "    morphisms and instance laws against the Lean kernel (ansatz);\n"
              "    wrap each migrate call with the check to ship a 'verified\n"
              "    migration' guarantee.\n"
              "  - persistence: switch (gg/weighted-graph) for a DatahikeACSet\n"
              "    constructor and the entire pipeline runs against a persistent\n"
              "    store with time-travel — same code, different backend."))))))
