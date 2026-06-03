(ns katzen.acset.homomorphism
  "Homomorphism search between two ACSets on the same schema.

   An ACSet homomorphism φ : A → B is a family of functions, one per
   object of the schema, that commutes with every morphism:

     for every hom m : Ob → Ob' in the schema and every part p in A,
     m_B(φ_Ob(p)) = φ_Ob'(m_A(p))

   This is the categorical condition for φ to be a natural
   transformation between the functors A, B : C → Set.

   Algorithm: backtracking CSP search with MRV variable ordering and
   *transitive forward propagation* — directly matching Catlab.jl's
   BacktrackingSearch (HomSearch.jl:337-472). When we tentatively
   assign φ_Ob(p) := q, every hom f : Ob → Ob' in the schema forces
   an immediate recursive assignment φ_Ob'(m_A(p)) := m_B(q), and so
   on transitively until the chain stops.

   Picking variables: at each step we score every unassigned (ob,p)
   by the count of *forward-feasible* target values — i.e. values
   that survive a trial propagation. This is MRV-with-feasibility,
   Catlab's `find_mrv_elem` + `can_assign_elem`.

   Same input/output contract as katzen.acset.homomorphism-datalog so
   the two engines can be benchmarked head-to-head."
  (:require [katzen.acset :as a]))

;; ============================================================================
;; Candidates and initial domain
;; ============================================================================
;;
;; A search state is a single map: candidates {[ob part-id] → #{q-in-tgt ...}}.
;; A singleton set means the variable is pinned. The search ends when every
;; variable is pinned.

(defn- initial-candidates
  "Every src-part starts with all parts of the same object in tgt."
  [src tgt]
  (let [sch (a/schema src)]
    (into {}
          (for [ob (:objects sch)
                p  (a/parts src ob)]
            [[ob p] (set (a/parts tgt ob))]))))

;; ============================================================================
;; Transitive forward propagation
;; ============================================================================

(declare propagate)

(defn- propagate
  "Tentatively bind [ob p] := q and transitively propagate every forward
   constraint. Returns updated candidates or nil on infeasibility.

   For each hom f : ob → ob' in the schema:
     - p' = m_A(p)   (source side; may be unset)
     - q' = m_B(q)   (target side; must be defined for the assignment
                       to extend, else infeasibility)
   We recursively call propagate for [ob' p'] := q'. The recursion
   bottoms out at objects with no outgoing homs or at already-consistent
   bindings.

   The whole step is functional — candidates is threaded through, never
   mutated. A backtracking caller can discard the returned value."
  [src tgt candidates ob p q]
  (let [cur (get candidates [ob p])]
    (cond
      ;; Already pinned to q — consistent. Nothing more to do.
      (and (= 1 (count cur)) (contains? cur q))
      candidates

      ;; q not in the current candidate set — infeasible.
      (not (contains? cur q))
      nil

      :else
      ;; Commit, then propagate.
      (loop [homs (:homs (a/schema src))
             c    (assoc candidates [ob p] #{q})]
        (cond
          (nil? c) nil
          (empty? homs) c
          :else
          (let [{:keys [name dom codom]} (first homs)
                rest-homs (rest homs)]
            (if (not= ob dom)
              (recur rest-homs c)
              (let [p' (a/subpart src name p)]
                (if (nil? p')
                  (recur rest-homs c)            ;; m_A(p) unset — no constraint
                  (let [q' (a/subpart tgt name q)]
                    (if (nil? q')
                      nil                        ;; m_B(q) unset — can't extend
                      (let [c' (propagate src tgt c codom p' q')]
                        (if c'
                          (recur rest-homs c')
                          nil)))))))))))))

;; ============================================================================
;; MRV variable ordering — counts forward-feasible values per variable
;; ============================================================================

(defn- feasible-set
  "Subset of cur values for [ob p] that survive trial-propagation. The
   propagation is functional, so we drop the trial result and only keep
   the value. Catlab's can_assign_elem mutates then unassigns; ours just
   evaluates a pure call."
  [src tgt candidates ob p cur]
  (into #{}
        (filter #(some? (propagate src tgt candidates ob p %)))
        cur))

(defn- pick-variable
  "Pick the unassigned [ob p] with the smallest set of forward-feasible
   values. Returns [[ob p] feasible-set], or nil if no variables remain
   unassigned (every candidate is a singleton).

   An empty cur-set (no possible values at all) counts as unassigned;
   pick-variable returns it with feas=∅ so the search will fail and
   the parent will backtrack."
  [src tgt candidates]
  (let [unassigned (filter (fn [[_ cs]] (not= 1 (count cs))) candidates)]
    (when (seq unassigned)
      (loop [items unassigned
             best-k nil
             best-feas nil
             best-cnt Long/MAX_VALUE]
        (if (empty? items)
          (when best-k [best-k best-feas])
          (let [[k cs] (first items)
                [ob p] k
                feas   (feasible-set src tgt candidates ob p cs)
                cnt    (count feas)]
            (cond
              ;; Domain wipeout — pick this immediately, search will fail.
              (zero? cnt)
              [k feas]

              (< cnt best-cnt)
              (recur (rest items) k feas cnt)

              :else
              (recur (rest items) best-k best-feas best-cnt))))))))

;; ============================================================================
;; Recursive search
;; ============================================================================

(defn- candidates->assignment
  "Once every candidate set is a singleton, project to the assignment map."
  [candidates]
  (into {} (map (fn [[k cs]] [k (first cs)])) candidates))

(defn- search
  "Recursive backtracking.
     mode = :one → first complete assignment, or nil
     mode = :all → seq of all complete assignments"
  [src tgt candidates mode]
  (if-let [[k feasible] (pick-variable src tgt candidates)]
    (let [[ob p] k
          step    (fn [q]
                    (when-let [c' (propagate src tgt candidates ob p q)]
                      (search src tgt c' mode)))]
      (case mode
        :one (some step feasible)
        :all (mapcat #(or (step %) []) feasible)))
    (case mode
      :one (candidates->assignment candidates)
      :all [(candidates->assignment candidates)])))

;; ============================================================================
;; Public API
;; ============================================================================

(defn homomorphism
  "Find one homomorphism from src to tgt, or nil. Returns an assignment
   map {[ob part-id-in-src] → part-id-in-tgt}."
  [src tgt]
  (when-not (= (a/schema src) (a/schema tgt))
    (throw (ex-info "homomorphism: schemas don't match"
                    {:src-schema (-> src a/schema :name)
                     :tgt-schema (-> tgt a/schema :name)})))
  (search src tgt (initial-candidates src tgt) :one))

(defn homomorphisms
  "All homomorphisms src → tgt as a seq of assignment maps."
  [src tgt]
  (when-not (= (a/schema src) (a/schema tgt))
    (throw (ex-info "homomorphisms: schemas don't match"
                    {:src-schema (-> src a/schema :name)
                     :tgt-schema (-> tgt a/schema :name)})))
  (search src tgt (initial-candidates src tgt) :all))

(defn nhomomorphisms
  "Count homomorphisms without retaining them. Useful for benchmarks."
  [src tgt]
  (count (homomorphisms src tgt)))

(defn assignment->per-object
  "Turn {[ob p] → q} into {ob {p → q}}."
  [assignment]
  (->> assignment
       (group-by ffirst)
       (reduce-kv (fn [m ob entries]
                    (assoc m ob (into {} (map (fn [[[_ p] q]] [p q]) entries))))
                  {})))
