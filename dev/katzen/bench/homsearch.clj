(ns katzen.bench.homsearch
  "Benchmark harness comparing homomorphism-search engines:

   1. bt-vec   — backtracker / VectorACSet  (in-memory, no deps)
   2. bt-dh    — backtracker / DatahikeACSet (same algorithm, slower protocol ops)
   3. dl-leg   — datalog / DatahikeACSet, legacy datahike engine
   4. dl-plan  — datalog / DatahikeACSet, datahike query planner

   The engines all read from ACSets through the IACSet protocol so they
   are interchangeable as functions of (src, tgt). ACSets are built ONCE
   per problem (outside the timing) so we measure search, not setup.

   Engines 3 and 4 differ only by a `(binding [dq/*force-legacy* …])`.

   Each problem records a *reference count* (the value all correct
   engines should produce). The reference is bt-vec's result, since
   the backtracker has been validated against the Catlab algorithm and
   against the existing test suite. Engines that disagree are flagged.

   Run:
     clojure -M:bench
     (require 'katzen.bench.homsearch)
     (katzen.bench.homsearch/run!)"
  (:require [datahike.query :as dq]
            [katzen.acset :as a]
            [katzen.acset.datahike :as dh]
            [katzen.acset.homomorphism :as hom]
            [katzen.acset.homomorphism-datalog :as homd]))

;; ============================================================================
;; Problem builders — return ACSets, not builders
;; ============================================================================

(defn- ->path
  "Path 1 → 2 → … → n using the given graph constructor."
  [graph-ctor n]
  (let [[g vs] (a/add-vertices (graph-ctor) n)]
    (reduce (fn [g i]
              (first (a/add-edge g (nth vs i) (nth vs (inc i)))))
            g
            (range (dec n)))))

(defn- ->cycle [graph-ctor n]
  (let [g     (->path graph-ctor n)
        verts (vec (a/vertices g))]
    (first (a/add-edge g (peek verts) (first verts)))))

(defn- ->complete
  "Complete digraph K_n with self-loops: n vertices, n² edges."
  [graph-ctor n]
  (let [[g vs] (a/add-vertices (graph-ctor) n)]
    (reduce (fn [g [i j]]
              (first (a/add-edge g (nth vs i) (nth vs j))))
            g
            (for [i (range n) j (range n)] [i j]))))

(defn- ->random-digraph [graph-ctor n m seed]
  (let [rng (java.util.Random. seed)
        [g vs] (a/add-vertices (graph-ctor) n)]
    (reduce (fn [g _]
              (let [i (.nextInt rng n)
                    j (.nextInt rng n)]
                (first (a/add-edge g (nth vs i) (nth vs j)))))
            g
            (range m))))

(defn- ->single-edge [graph-ctor]
  (let [[g vs] (a/add-vertices (graph-ctor) 2)
        [g _]  (a/add-edge g (first vs) (second vs))]
    g))

;; ============================================================================
;; Engine functions
;; ============================================================================

(defn- engine-bt [src tgt]
  (hom/nhomomorphisms src tgt))

(defn- engine-dl-legacy [src tgt]
  (binding [dq/*force-legacy* true]
    (homd/nhomomorphisms-datalog src tgt)))

(defn- engine-dl-planner [src tgt]
  (binding [dq/*force-legacy* false]
    (homd/nhomomorphisms-datalog src tgt)))

;; ============================================================================
;; Problem catalogue — each problem provides builders for BOTH backends
;; ============================================================================

(def ^:private problems
  [{:name "empty → triangle"
    :build-vec (fn [] [(a/graph) (->cycle a/graph 3)])
    :build-dh  (fn [] [(dh/graph) (->cycle dh/graph 3)])}

   {:name "single edge → triangle"
    :build-vec (fn [] [(->single-edge a/graph) (->cycle a/graph 3)])
    :build-dh  (fn [] [(->single-edge dh/graph) (->cycle dh/graph 3)])}

   {:name "single edge → K3 (loops)"
    :build-vec (fn [] [(->single-edge a/graph) (->complete a/graph 3)])
    :build-dh  (fn [] [(->single-edge dh/graph) (->complete dh/graph 3)])}

   {:name "path-3 → triangle"
    :build-vec (fn [] [(->path a/graph 3) (->cycle a/graph 3)])
    :build-dh  (fn [] [(->path dh/graph 3) (->cycle dh/graph 3)])}

   {:name "cycle-3 → cycle-6"
    :build-vec (fn [] [(->cycle a/graph 3) (->cycle a/graph 6)])
    :build-dh  (fn [] [(->cycle dh/graph 3) (->cycle dh/graph 6)])}

   {:name "cycle-4 → cycle-8"
    :build-vec (fn [] [(->cycle a/graph 4) (->cycle a/graph 8)])
    :build-dh  (fn [] [(->cycle dh/graph 4) (->cycle dh/graph 8)])}

   {:name "triangle → K4 (loops)"
    :build-vec (fn [] [(->cycle a/graph 3) (->complete a/graph 4)])
    :build-dh  (fn [] [(->cycle dh/graph 3) (->complete dh/graph 4)])}

   {:name "triangle → K5 (loops)"
    :build-vec (fn [] [(->cycle a/graph 3) (->complete a/graph 5)])
    :build-dh  (fn [] [(->cycle dh/graph 3) (->complete dh/graph 5)])}

   {:name "path-4 → random(8,16)"
    :build-vec (fn [] [(->path a/graph 4) (->random-digraph a/graph 8 16 42)])
    :build-dh  (fn [] [(->path dh/graph 4) (->random-digraph dh/graph 8 16 42)])}

   {:name "path-5 → random(12,30)"
    :build-vec (fn [] [(->path a/graph 5) (->random-digraph a/graph 12 30 7)])
    :build-dh  (fn [] [(->path dh/graph 5) (->random-digraph dh/graph 12 30 7)])}])

;; ============================================================================
;; Timing
;; ============================================================================

(defn- time-ms
  "Run f a few times after a warmup. Return median wall time (ms)."
  [f]
  ;; Warmup
  (dotimes [_ 2] (f))
  (let [runs (vec (repeatedly 5 (fn []
                                  (let [t0 (System/nanoTime)]
                                    (f)
                                    (/ (- (System/nanoTime) t0) 1e6)))))
        sorted (sort runs)]
    (nth sorted 2)))

;; ============================================================================
;; Runner
;; ============================================================================

(defn- run-problem [{:keys [name build-vec build-dh]}]
  (let [[src-v tgt-v] (build-vec)
        [src-d tgt-d] (build-dh)
        ;; Reference: bt-vec is our ground truth.
        ref-count (engine-bt src-v tgt-v)
        ;; Each engine gets (src, tgt) appropriate for its backend.
        results
        [["bt-vec"   ref-count                          (time-ms #(engine-bt src-v tgt-v))]
         ["bt-dh"    (engine-bt src-d tgt-d)            (time-ms #(engine-bt src-d tgt-d))]
         ["dl-leg"   (engine-dl-legacy src-d tgt-d)     (time-ms #(engine-dl-legacy src-d tgt-d))]
         ["dl-plan"  (engine-dl-planner src-d tgt-d)    (time-ms #(engine-dl-planner src-d tgt-d))]]]
    {:name name :ref ref-count :results results}))

(defn- format-cell
  "Print a cell showing the per-engine time, with a ⚠ if its count disagrees."
  [ref-count [label cnt ms]]
  (format "%s%-10s"
          (if (= cnt ref-count) " " "⚠")
          (format "%.1fms" ms)))

(defn run!
  "Run every problem with all four engines, print a table."
  []
  (println "\nHomomorphism-search benchmark — median wall time over 5 runs.")
  (println "Reference count = bt-vec's result; ⚠ on an engine = count disagreement with reference.\n")
  (printf "%-32s | %-10s %-10s %-10s %-10s | ref count\n"
          "problem" "bt-vec" "bt-dh" "dl-leg" "dl-plan")
  (println (apply str (repeat 90 \-)))
  (let [reports (mapv run-problem problems)]
    (doseq [{:keys [name ref results]} reports]
      (printf "%-32s | %s%s%s%s | %d\n"
              name
              (format-cell ref (nth results 0))
              (format-cell ref (nth results 1))
              (format-cell ref (nth results 2))
              (format-cell ref (nth results 3))
              ref))
    (println)
    ;; Disagreement summary
    (let [disagrees
          (for [{:keys [name ref results]} reports
                [label cnt _] results
                :when (not= cnt ref)]
            [name label ref cnt])]
      (when (seq disagrees)
        (println "Disagreements (engine ⇒ count vs reference):")
        (doseq [[name label ref cnt] disagrees]
          (printf "  %-32s  %-10s  expected %d, got %d\n"
                  name label ref cnt))))
    reports))
