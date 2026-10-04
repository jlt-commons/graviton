(ns graviton.physics-test
  (:require [clojure.test :refer [deftest is testing]]
            [graviton.physics :as phys]))

(defn near
  [a b]
  (< (Math/abs (- a b)) 1e-9))

(defn nearish
  "Tolerance that admits the r-squared epsilon's 1e-7 nudge."
  [a b]
  (< (Math/abs (- a b)) 1e-6))

(defn near-v
  [[ax ay] [bx by]]
  (and (near ax bx) (near ay by)))

(deftest nearest-point-snaps-to-the-offset-lattice
  (testing "points map to the nearest lattice site, offset by the radius"
    (is (near (phys/nearest-point 20 45) 60))
    (is (near (phys/nearest-point 20 30) 20))
    (is (near (phys/nearest-point 20 0) 20) "equidistant rounds up")
    (is (near (phys/nearest-point 20 -45) -60))))

(deftest attractor-accel-points-toward-the-attractor
  (let [a {:x 100 :y 100 :mass 50 :radius 10}
        [ax ay] (phys/attractor-accel a 140 100)]
    (is (neg? ax) "point right of attractor pulls left")
    (is (near ay 0) "level with the attractor, no vertical pull"))
  (let [a {:x 0 :y 0 :mass 10 :radius 10}
        [ax ay] (phys/attractor-accel a -3 -4)]
    (is (pos? ax))
    (is (pos? ay))))

(deftest attractor-accel-magnitude-is-mass-times-radius-over-r-squared
  (let [a {:x 0 :y 0 :mass 10 :radius 10}
        [ax ay] (phys/attractor-accel a 10 0)]
    (is (nearish ax -1.0) "mass*radius/r^2 along the axis, pointing at the attractor")
    (is (near ay 0))))

(deftest a-bigger-source-pulls-harder-at-the-same-distance
  (let [small {:x 0 :y 0 :mass 50 :radius 10}
        big   {:x 0 :y 0 :mass 50 :radius 20}
        [ax _] (phys/attractor-accel small 30 0)
        [bx _] (phys/attractor-accel big 30 0)]
    (is (nearish ax (- (/ 500.0 900.0))))
    (is (nearish bx (- (/ 1000.0 900.0))))
    (is (< (Math/abs ax) (Math/abs bx)) "size scales the pull")))

(deftest accel-at-with-no-attractors-is-zero
  (is (near-v (phys/accel-at [] 55 66) [0.0 0.0])))

(deftest accel-at-superposes-attractors
  (let [a1 {:x 0 :y 0 :mass 1 :radius 1}
        a2 {:x 100 :y 0 :mass 1 :radius 1}
        [s-x s-y] (phys/accel-at [a1 a2] 40 30)
        [x1 y1] (phys/attractor-accel a1 40 30)
        [x2 y2] (phys/attractor-accel a2 40 30)]
    (is (near s-x (+ x1 x2)))
    (is (near s-y (+ y1 y2)))))

(deftest accel-at-clamps-the-magnitude
  (let [a {:x 0 :y 0 :mass 1000000 :radius 1}
        [ax ay] (phys/accel-at [a] 5 5)
        mag (Math/sqrt (+ (* ax ax) (* ay ay)))]
    (is (near mag 1.0) "never accelerates harder than MAX-ACCEL")))

(deftest a-close-monster-well-cannot-drown-out-opposing-wells
  (let [monster {:x 0 :y 0 :mass 10000 :radius 100}
        helper {:x 30 :y 0 :mass 120 :radius 2}
        [ax ay] (phys/accel-at [monster helper helper] 10 0)]
    (is (near ay 0) "the lineup is collinear")
    (is (nearish ax 0.2)
        "each well is capped before the sum: monster -1, helpers +0.6 each")))

(deftest wrapping-the-right-edge-preserves-velocity
  (let [s (phys/integrate {:x 795 :y 300 :vx 20 :vy 0} 0 0 1.0 800 600)]
    (is (near (:x s) 15.0) "leaves the right edge, re-enters from the left")
    (is (near (:vx s) 20.0) "wrapping does not damp velocity")))

(deftest wrapping-the-left-edge
  (let [s (phys/integrate {:x 5 :y 300 :vx -20 :vy 0} 0 0 1.0 800 600)]
    (is (near (:x s) 785.0) "re-enters from the right")))

(deftest wrapping-the-top-edge
  (let [s (phys/integrate {:x 400 :y 5 :vx 0 :vy -10} 0 0 1.0 800 600)]
    (is (near (:y s) 595.0) "re-enters from the bottom")))

(deftest wrapping-the-bottom-edge
  (let [s (phys/integrate {:x 400 :y 595 :vx 0 :vy 10} 0 0 1.0 800 600)]
    (is (near (:y s) 5.0) "re-enters from the top")))
