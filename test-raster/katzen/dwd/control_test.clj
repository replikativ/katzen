(ns katzen.dwd.control-test
  "The directed/machine layer as a control example — a port of
   AlgebraicDynamics.jl's `examples/Cyber-Physical.jl` (Bakirtzis et al.,
   *Categorical Semantics of Cyber-Physical Systems Theory*): a UAV pitch
   CLOSED LOOP built from three machines wired with feedback.

     sensor     : 1 state, ins [θ(feedback), e(setpoint)], out = state
     controller : 1 state, ins [sensor-out, d(setpoint)],  out = state
     plant      : 3 states [α q θ], in [control], out = θ
     wires      : sensor→controller→plant→sensor (feedback) ; plant→outer-out
     outer      : ins [e d], out [θ]

   The acceptance test is composition correctness: the machine composed
   via `oapply-dwd` (with a feedback wire and two driving inputs) must
   integrate to the *same* trajectory as the hand-written monolithic
   5-state ODE — proving the directed operad algebra wires feedback and
   inputs correctly. Plus the fast-loop sanity limits (sensor tracks θ+e,
   controller tracks d)."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.compile.core :as cc]
            [katzen.dwd :as dwd]
            [katzen.dwd.dynamics :as mach]
            [katzen.petri :as p]
            ;; raster namespaces so emitted raster forms resolve under compile-rhs
            [raster.core]
            [raster.numeric]
            [raster.arrays]))

(def Al 100.0)  ; sensor decay constant
(def Ac 100.0)  ; controller decay constant
(def Bc 0.0)    ; controller velocity/reference ratio
(def E 0.01)    ; outer input e (θ offset setpoint)
(def D 0.05)    ; outer input d (control setpoint)

(defn- sensor []
  (mach/raw-machine
   {:state-labels [:sc] :inputs 2 :outputs 1
    :dynamics (fn [[sc] [x1 x2] _] [(* (- Al) (- sc x1 x2))])  ; x1=θ x2=e
    :readout  (fn [[sc] _] [sc])}))

(defn- controller []
  (mach/raw-machine
   {:state-labels [:sl] :inputs 2 :outputs 1
    :dynamics (fn [[sl] [x1 x2] _] [(* (- Ac) (- (+ sl (* Bc x1)) x2))])  ; x1=sc x2=d
    :readout  (fn [[sl] _] [sl])}))

(defn- plant []
  (mach/raw-machine
   {:state-labels [:alpha :q :theta] :inputs 1 :outputs 1
    :dynamics (fn [[a q _th] [c] _]
                [(+ (* -0.313 a) (* 56.7 q) (* 0.232 c))
                 (+ (* -0.013 a) (* -0.426 q) (* 0.0203 c))
                 (* 56.7 q)])
    :readout  (fn [[_a _q th] _] [th])}))

(defn- uav []
  (let [d (dwd/dwd)
        [d Bs si so]  (dwd/add-box-with-ports d 2 1)
        [d Bc2 ci co] (dwd/add-box-with-ports d 2 1)
        [d Bp pi po]  (dwd/add-box-with-ports d 1 1)
        [d oi-e]      (dwd/add-outer-in-port d)
        [d oi-d]      (dwd/add-outer-in-port d)
        [d oo]        (dwd/add-outer-out-port d)
        [d _] (dwd/add-input-wire  d oi-e (nth si 1))       ; e → sensor in2
        [d _] (dwd/add-input-wire  d oi-d (nth ci 1))       ; d → controller in2
        [d _] (dwd/add-box-wire    d (nth so 0) (nth ci 0)) ; sensor → controller in1
        [d _] (dwd/add-box-wire    d (nth co 0) (nth pi 0)) ; controller → plant in1
        [d _] (dwd/add-box-wire    d (nth po 0) (nth si 0)) ; plant → sensor in1 (FEEDBACK)
        [d _] (dwd/add-output-wire d (nth po 0) oo)]        ; plant → outer-out
    (mach/oapply-dwd d {Bs (sensor) Bc2 (controller) Bp (plant)})))

;; Hand-written monolith — states [sc sl α q θ], driven by e=E, d=D.
(defn- monolith [^doubles du ^doubles u _t]
  (let [sc (aget u 0) sl (aget u 1) a (aget u 2) q (aget u 3) th (aget u 4)]
    (aset du 0 (* (- Al) (- sc th E)))
    (aset du 1 (* (- Ac) (- (+ sl (* Bc sc)) D)))
    (aset du 2 (+ (* -0.313 a) (* 56.7 q) (* 0.232 sl)))
    (aset du 3 (+ (* -0.013 a) (* -0.426 q) (* 0.0203 sl)))
    (aset du 4 (* 56.7 q))))

(defn- end-state [rhs]
  (-> (p/integrate-rk4 rhs (double-array [0.0 0 0 0 0]) 0.0 20.0 0.0005)
      :us last vec))

(defn- end-state-2 [rhs]
  (-> (p/integrate-rk4 rhs (double-array [1.0 0.0]) 0.0 10.0 0.001)
      :us last vec))

(deftest uav-composite-shape
  (let [m (uav)]
    (is (= 2 (:n-inputs m))  "outer inputs e, d")
    (is (= 1 (:n-outputs m)) "outer output θ")
    (is (= 5 (:size (:layout m))) "disjoint union: sc, sl, α, q, θ")))

(deftest uav-composite-equals-monolith
  (testing "oapply-dwd wires feedback + inputs to exactly the intended coupled ODE"
    (let [end-c (end-state (mach/signal-rhs (uav) [E D]))
          end-m (end-state monolith)
          max-diff (apply max (map (fn [a b] (Math/abs (double (- a b)))) end-c end-m))]
      (is (< max-diff 1e-9) "composite trajectory == monolith")
      (testing "fast-loop tracking limits"
        (is (< (Math/abs (double (- (nth end-c 0) (+ (nth end-c 4) E)))) 1e-3)
            "sensor state tracks θ + e")
        (is (< (Math/abs (double (- (nth end-c 1) D))) 1e-3)
            "controller state tracks d")))))

(deftest open-machine-runtime
  (testing "eval-dynamics and readout drive a single open machine"
    (let [plant (plant)]
      ;; at rest with control input c=1: α̇ = 0.232, q̇ = 0.0203, θ̇ = 0
      (is (= [0.232 0.0203 0.0] (mach/eval-dynamics plant [0.0 0 0] [1.0] 0.0)))
      ;; readout is state-only: returns θ (3rd state)
      (is (= [7.0] (mach/readout plant [5.0 6.0 7.0] 0.0)))))
  (testing "signal-rhs accepts time-varying input signals"
    (let [plant (plant)
          rhs   (mach/signal-rhs plant [(fn [t] (Math/sin t))])
          du    (double-array 3)]
      (rhs du (double-array [0.0 0 0]) 0.0)        ; sin(0)=0 → no control contribution
      (is (= [0.0 0.0 0.0] (vec du))))))

(deftest input-arity-checked
  (is (thrown-with-msg? Exception #"wrong number of inputs"
                        (mach/eval-dynamics (plant) [0.0 0 0] [1.0 2.0] 0.0))))

;; ============================================================================
;; vector-machine — the same UAV built symbolically must match the raw one
;; ============================================================================

(defn- uav-vector []
  (let [sensor     (mach/vector-machine
                    {:state-labels '[sc] :inputs '[x1 x2]
                     :field '{sc (* -100.0 (- sc x1 x2))} :readout '[sc]})
        controller (mach/vector-machine
                    {:state-labels '[sl] :inputs '[x1 x2]   ; Bc=0 ⇒ x1 unused
                     :field '{sl (* -100.0 (- sl x2))} :readout '[sl]})
        plant      (mach/vector-machine
                    {:state-labels '[alpha q theta] :inputs '[c]
                     :field '{alpha (+ (* -0.313 alpha) (* 56.7 q) (* 0.232 c))
                              q     (+ (* -0.013 alpha) (* -0.426 q) (* 0.0203 c))
                              theta (* 56.7 q)}
                     :readout '[theta]})
        d (dwd/dwd)
        [d Bs si so]  (dwd/add-box-with-ports d 2 1)
        [d Bc2 ci co] (dwd/add-box-with-ports d 2 1)
        [d Bp pi po]  (dwd/add-box-with-ports d 1 1)
        [d oi-e]      (dwd/add-outer-in-port d)
        [d oi-d]      (dwd/add-outer-in-port d)
        [d oo]        (dwd/add-outer-out-port d)
        [d _] (dwd/add-input-wire  d oi-e (nth si 1))
        [d _] (dwd/add-input-wire  d oi-d (nth ci 1))
        [d _] (dwd/add-box-wire    d (nth so 0) (nth ci 0))
        [d _] (dwd/add-box-wire    d (nth co 0) (nth pi 0))
        [d _] (dwd/add-box-wire    d (nth po 0) (nth si 0))
        [d _] (dwd/add-output-wire d (nth po 0) oo)]
    (mach/oapply-dwd d {Bs sensor Bc2 controller Bp plant})))

(deftest vector-machine-uav-equals-raw
  (testing "symbolic machines compose to the same closed-loop trajectory as raw ones"
    (let [end-v (end-state (mach/signal-rhs (uav-vector) [E D]))
          end-r (end-state (mach/signal-rhs (uav) [E D]))]
      (is (< (apply max (map (fn [a b] (Math/abs (double (- a b)))) end-v end-r)) 1e-9)
          "vector-machine UAV == raw-machine UAV"))))

;; ============================================================================
;; vector-machine raster path — a CLOSED machine compiles both ways alike
;; ============================================================================

(deftest vector-machine-raster-equals-clojure
  (testing "a closed (no-input) symbolic machine: raster body == clojure body"
    (let [osc (mach/vector-machine            ; damped oscillator, no inputs
               {:state-labels '[x v]
                :field '{x v, v (+ (* -1.0 x) (* -0.1 v))}
                :readout '[x]})
          ras (end-state-2 (cc/compile-rhs osc))
          clj (end-state-2 (cc/compile-clojure-rhs osc))]
      (is (< (apply max (map (fn [a b] (Math/abs (double (- a b)))) ras clj)) 1e-9)
          "raster-compiled vector-machine == clojure-compiled"))))
