(ns katzen.compose-test
  "The unified `katzen.compose/oapply` must route to the same per-operad
   law as the named functions, dispatching on (operad × algebra)."
  (:require [clojure.test :refer [deftest is testing]]
            [katzen.compile.core :as cc]
            [katzen.compose :as kc]
            [katzen.dwd :as dwd]
            [katzen.dwd.dynamics :as mach]
            [katzen.ode :as ode]
            [katzen.petri :as p]
            [katzen.uwd :as uwd]
            [katzen.uwd.dynamics :as ud]))

(defn- uwd-diagram []
  (let [d (uwd/uwd) [d [jR jF]] (uwd/add-junctions d 2)
        [d Bg _] (uwd/add-box-with-ports d [jR])
        [d Bp _] (uwd/add-box-with-ports d [jR jF])
        [d Bd _] (uwd/add-box-with-ports d [jF])]
    {:d d :boxes {Bg (ud/from-compilable (ode/vector-field {:states '[r] :params '{a 1.1} :field '{r (* a r)}}) '[r])
                  Bp (ud/from-compilable (ode/vector-field {:states '[r f] :params '{b 0.4 d 0.1}
                                                            :field '{r (- (* b r f)) f (* d r f)}}) '[r f])
                  Bd (ud/from-compilable (ode/vector-field {:states '[f] :params '{g 0.4} :field '{f (- (* g f))}}) '[f])}}))

(defn- dwd-diagram []
  (let [plant (mach/raw-machine {:state-labels [:x] :inputs 1
                                 :dynamics (fn [[x] [u] _] [u]) :readout (fn [[x] _] [x])})
        ctrl  (mach/raw-machine {:state-labels [:c] :inputs 2
                                 :dynamics (fn [[c] [x r] _] [(* 10.0 (- (- r x) c))]) :readout (fn [[c] _] [c])})
        d (dwd/dwd)
        [d Bp pin pout] (dwd/add-box-with-ports d 1 1)
        [d Bc cin cout] (dwd/add-box-with-ports d 2 1)
        [d r] (dwd/add-outer-in-port d) [d y] (dwd/add-outer-out-port d)
        [d _] (dwd/add-box-wire    d (nth cout 0) (nth pin 0))
        [d _] (dwd/add-box-wire    d (nth pout 0) (nth cin 0))
        [d _] (dwd/add-input-wire  d r (nth cin 1))
        [d _] (dwd/add-output-wire d (nth pout 0) y)]
    {:d d :boxes {Bp plant Bc ctrl}}))

(deftest oapply-routes-uwd-resource-sharer
  (testing "[SchUWD :resource-sharer] → uwd.dynamics/oapply"
    (let [{:keys [d boxes]} (uwd-diagram)
          via-multi  (kc/oapply d boxes)
          via-direct (ud/oapply d boxes)
          end (fn [crs] (vec (last (:us (p/integrate-rk4 (cc/compile-clojure-rhs crs)
                                                         (double-array [10.0 10.0]) 0.0 20.0 0.001)))))]
      (is (= 2 (:size (cc/layout-of via-multi))) "composed to a 2-state CRS")
      (is (= (end via-multi) (end via-direct)) "same result as the named fn"))))

(deftest oapply-routes-dwd-machine
  (testing "[SchDWD :machine] → dwd.dynamics/oapply-dwd"
    (let [{:keys [d boxes]} (dwd-diagram)
          via-multi  (kc/oapply d boxes)
          via-direct (mach/oapply-dwd d boxes)
          end (fn [m] (vec (last (:us (p/integrate-rk4 (mach/signal-rhs m [1.0])
                                                       (double-array [0.0 0.0]) 0.0 10.0 0.01)))))]
      (is (mach/machine? via-multi) "composed to a Machine")
      (is (= (end via-multi) (end via-direct)) "same result as the named fn"))))

(deftest oapply-errors
  (testing "empty box map can't infer the algebra"
    (is (thrown-with-msg? Exception #"empty box map"
                          (kc/oapply (:d (uwd-diagram)) {}))))
  (testing "unrecognized algebra value"
    (is (thrown-with-msg? Exception #"unrecognized box-system algebra"
                          (kc/oapply (:d (uwd-diagram)) {0 :not-a-system})))))
