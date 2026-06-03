(ns katzen.test-support
  "Helpers shared across tests for handling the optional-backend pattern.

   Most of katzen's heavyweight dependencies (datahike, raster,
   ansatz) live behind opt-in aliases and aren't on the classpath
   during the default `:test` run. Tests that need them want to:

     - run when the backend is available,
     - print a single-line `[skip]` notice and pass trivially otherwise.

   The predicates and `skip-notice` helper here cover both cases. They
   replace ad-hoc per-file definitions that drifted across
   `test/`, `test-raster/`, `test-ansatz/`."
  (:require [clojure.test :refer [is]]))

;; ============================================================================
;; Backend availability
;; ============================================================================

(def datahike-available?
  "True when `katzen.acset.datahike` (which requires the `:datahike` alias)
   is resolvable."
  (some? (try (requiring-resolve 'katzen.acset.datahike/datahike-acset)
              (catch Throwable _ nil))))

(def raster-available?
  "True when raster's ODE solver is resolvable — i.e. the `:raster` alias
   is on the classpath."
  (some? (try (requiring-resolve 'raster.ode/solve)
              (catch Throwable _ nil))))

;; ============================================================================
;; Ansatz availability
;; ============================================================================
;;
;; Ansatz needs both the JAR on the classpath AND a pre-built Mathlib
;; store at /var/tmp/ansatz-mathlib. The standard test pattern is:
;;   1. (use-fixtures :once ensure-ansatz-init!)
;;   2. Every deftest body wraps its work in `(when (ansatz-ready?) ...)`.

(def ansatz-store-path "/var/tmp/ansatz-mathlib")

(defn ansatz-ready?
  "True when the Mathlib store exists locally."
  []
  (.exists (java.io.File. ansatz-store-path)))

(defn ensure-ansatz-init!
  "Test fixture: initialize ansatz against the Mathlib store if it's
   present and not already initialized. No-op when the store is absent.
   Use as `(use-fixtures :once ensure-ansatz-init!)`."
  [f]
  (when (ansatz-ready?)
    (let [init!  (requiring-resolve 'ansatz.core/init!)
          env-fn (requiring-resolve 'ansatz.core/env)]
      (when-not (try (env-fn) (catch Throwable _ nil))
        (init! ansatz-store-path "mathlib"))))
  (f))

;; ============================================================================
;; Skip pattern
;; ============================================================================

(defn skip-notice
  "Print a `[skip] <tag>` line and pass a trivial assertion. Use in the
   else-branch of an availability check inside a `deftest`."
  [tag]
  (println (str "  [skip] " tag " not available"))
  (is true))
