(ns katzen.acset.homomorphism-datalog
  "Bottom-up datalog implementation of ACSet homomorphism search.

   The constraints are encoded as a single datahike query over the
   TARGET database. Each part of the SOURCE becomes a free logic
   variable; object membership clauses pin it to the right object
   class; each morphism in the source becomes a constraint on the
   corresponding logic variables in the target.

   This is the other engine in the comparison set up in
   katzen.acset.homomorphism. Same input/output contract."
  (:require [datahike.api :as d]
            [katzen.acset :as a]))

(defn- part-lvar
  "Stable logic variable for source part (ob, p). Datahike datalog
   variables are symbols starting with ?."
  [ob p]
  (symbol (str "?" (name ob) p)))

(defn- build-query
  "Build the datalog `:find ... :where ...` form for the hom-search."
  [src]
  (let [sch (a/schema src)
        all-parts (vec (for [ob (:objects sch)
                             p  (a/parts src ob)]
                         [ob p]))
        find-syms (mapv (fn [[ob p]] (part-lvar ob p)) all-parts)
        ;; Object marker clauses — each part variable must be that kind.
        ob-clauses (mapv (fn [[ob p]]
                           [(part-lvar ob p) :katzen/ob ob])
                         all-parts)
        ;; Hom morphism clauses — each source edge constrains [src tgt]
        ;; of the corresponding target edge variable.
        hom-clauses
        (vec
         (for [{:keys [name dom codom]} (:homs sch)
               p     (a/parts src dom)
               :let  [q (a/subpart src name p)]
               :when (some? q)]
           [(part-lvar dom p) name (part-lvar codom q)]))
        where (vec (concat ob-clauses hom-clauses))]
    {:all-parts all-parts
     :query     (vec (concat [:find] find-syms [:where] where))}))

(defn- tuple->assignment
  [all-parts tuple]
  (into {} (map vector all-parts tuple)))

(defn- query-tgt
  "Run the query against the target's underlying db."
  [{:keys [conn] :as _tgt} query]
  (when (nil? conn)
    (throw (ex-info "datalog hom search needs a DatahikeACSet target (got no :conn)" {})))
  (d/q query (d/db conn)))

(defn homomorphisms-datalog
  "Find all homomorphisms src → tgt via datahike datalog. tgt must be a
   DatahikeACSet. Returns a seq of assignment maps {[ob p] → tgt-eid}."
  [src tgt]
  (when-not (= (a/schema src) (a/schema tgt))
    (throw (ex-info "homomorphisms-datalog: schemas don't match" {})))
  (let [{:keys [all-parts query]} (build-query src)]
    (if (empty? all-parts)
      ;; Vacuous: an empty src has exactly one (empty) homomorphism into anything.
      [{}]
      (let [results (query-tgt tgt query)]
        (map (partial tuple->assignment all-parts) results)))))

(defn homomorphism-datalog
  "Find one homomorphism via datalog (or nil)."
  [src tgt]
  (first (homomorphisms-datalog src tgt)))

(defn nhomomorphisms-datalog
  "Count homomorphisms via datalog. Useful for benchmarks."
  [src tgt]
  (count (homomorphisms-datalog src tgt)))
