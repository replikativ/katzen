(ns katzen.schema.clojure-code
  "The CLOJURE-CODE schema as a canonical, backend-agnostic katzen ACSet schema
   (part of `katzen.schema.*`). A PROJECTION of source text at the interface
   level: namespaces and top-level definitions keyed by a `qname` Identity, the
   relationships between them (which namespace a def lives in, what a def
   references, multimethod and protocol wiring), and the interface facts you'd
   query or render. Shared by dvergr (the in-memory code index) and simmis
   (rendering / cross-reference).

   This is a *projection*, not a faithful AST — the text/CST is ground truth (see
   dvergr's `diff-source` + write-back lens). It deliberately stops at the
   interface level: sub-form structure, locals, and dataflow belong to a separate
   semantic-AST view (the tools.analyzer AST imported as an ACSet), not here.

   Categorical shape:
     - Object `Namespace` — a Clojure namespace.
     - Object `Def`       — a top-level definition.
     - Hom `def-ns`    : Def → Namespace — the namespace a def lives in.
     - Hom `method-of` : Def → Def — a `defmethod` def's `defmulti` (PARTIAL —
       set only on `:defmethod` defs; carries the `dispatch-val` attr).
     - Attr `qname` : Def → Identity — the def's URI, the shared cross-ACSet join
       key. Resolving which refs land on a project Def is a pullback over
       `Identity` (`katzen.xref`); find-references is `incident :refs`.

   Two populations, merged by `qname`:
     - SYNTACTIC (raw form head + meta; always available): kind, qname, source,
       file, line, doc, private?, dispatch-val, implements.
     - SEMANTIC  (tools.analyzer on a loadable ns): refs (resolved callee
       qnames — the accurate call graph), arities, variadic?.

   Names are ABSTRACT. dvergr binds them to its `:def/*` / `:ns/*` idents.

   No structural path equation ships here: the natural invariant — \"a def lives
   in its declared namespace\" — compares the namespace *part of* `qname` (a
   string operation) against `def-ns ⋅ ns-name`, which is a type-side **Bool
   validation** (`katzen.eval`), not a path equation between morphisms. Left to
   the consuming app to assert via the type-side."
  (:require [katzen.acset :as a]))

(def schema
  "Canonical Clojure-code ACSet schema (abstract names) — interface-level L2."
  {:name       :ClojureCode
   :objects    [:Namespace :Def]
   :homs       [{:name :def-ns    :dom :Def :codom :Namespace :cardinality :one}
                ;; PARTIAL: only :defmethod defs carry it (→ their defmulti)
                {:name :method-of :dom :Def :codom :Def       :cardinality :one}]
   :attr-types [:Identity :String :Keyword :Long :Bool]
   :attrs
   [;; --- Def: syntactic (always available) ---
    {:name :qname        :dom :Def :codom :Identity}
    {:name :kind         :dom :Def :codom :Keyword}   ; :defn :def :defmacro :defmulti
                                                      ;   :defmethod :defprotocol :deftype
                                                      ;   :defrecord :extend :ns …
    {:name :source       :dom :Def :codom :String}    ; raw text — ground truth
    {:name :file         :dom :Def :codom :String}
    {:name :line         :dom :Def :codom :Long}
    {:name :doc          :dom :Def :codom :String}
    {:name :private?     :dom :Def :codom :Bool}
    {:name :dispatch-val :dom :Def :codom :String}    ; for :defmethod (with method-of)
    {:name :implements   :dom :Def :codom :Identity :cardinality :many} ; type/record → protocol qnames
    ;; --- Def: semantic (only when the ns is loadable) ---
    {:name :refs         :dom :Def :codom :Identity :cardinality :many} ; resolved callee qnames
    {:name :arities      :dom :Def :codom :Long     :cardinality :many} ; fixed arities
    {:name :variadic?    :dom :Def :codom :Bool}
    ;; --- Namespace ---
    {:name :ns-name      :dom :Namespace :codom :Identity}
    {:name :requires     :dom :Namespace :codom :Identity :cardinality :many}
    {:name :ns-doc       :dom :Namespace :codom :String}]
   :equations []})

(def identity-attr
  "The Attr carrying a Def's shared cross-ACSet Identity (its qname URI)."
  :qname)
