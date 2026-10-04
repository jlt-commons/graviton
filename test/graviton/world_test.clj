(ns graviton.world-test
  (:require [clojure.test :refer [deftest is testing]]
            [graviton.world :as w]))

(def W 800)
(def H 600)

(defn- world
  [& {:keys [prizes attractors zones ship]}]
  (merge {:width W :height H
          :total-prizes (count (or prizes []))
          :prizes []
          :attractors []
          :deathzones []
          :ship (merge {:x 400 :y 300 :vx 0 :vy 0 :radius 10} ship)}
         (when prizes {:prizes prizes})
         (when attractors {:attractors attractors})
         (when zones {:deathzones zones})))

(deftest drift-without-attractors-keeps-velocity
  (let [w0 (world :ship {:vx 5 :vy -3})
        w1 (w/step w0 1.0)]
    (is (= 405.0 (:x (:ship w1))))
    (is (= 297.0 (:y (:ship w1))))
    (is (= 5.0 (:vx (:ship w1))))))

(deftest step-collects-a-overlapped-prize
  (let [w0 (world :prizes [{:x 410 :y 300 :radius 40}])
        w1 (w/step w0 1.0)]
    (is (empty? (:prizes w1)))
    (is (= 1 (:score w1)))))

(deftest step-keeps-distant-prizes
  (let [w0 (world :prizes [{:x 100 :y 100 :radius 20}])
        w1 (w/step w0 1.0)]
    (is (= 1 (count (:prizes w1))))
    (is (zero? (:score w1)))))

(deftest collecting-the-last-prize-wins
  (let [w0 (world :prizes [{:x 410 :y 300 :radius 40}] :ship {:vx 5})
        w1 (w/step w0 1.0)]
    (is (= :won (:phase w1)))))

(deftest entering-a-deathzone-loses
  (let [w0 (world :zones [{:x 400 :y 300 :radius 50}])
        w1 (w/step w0 1.0)]
    (is (= :lost (:phase w1)))))

(deftest the-round-freezes-once-lost
  (let [w0 (world :zones [{:x 400 :y 300 :radius 50}] :ship {:vx 5})
        w1 (w/step w0 1.0)]
    (is (= :lost (:phase w1)))
    (is (= w1 (w/step w1 1.0)) "after the loss the round is frozen")))

(deftest the-round-freezes-once-won
  (let [w0 (world :prizes [{:x 410 :y 300 :radius 40}])
        w1 (w/step w0 1.0)]
    (is (= :won (:phase w1)))
    (is (= w1 (w/step w1 1.0)) "after the win the round is frozen")))

(deftest play-continues-otherwise
  (let [w1 (w/step (world) 0.5)]
    (is (= :playing (:phase w1)))))

(deftest add-attractor-merges-into-world
  (let [w0 (world)
        w1 (w/add-attractor w0 100 200 60)]
    (is (= 1 (count (:attractors w1))))
    (is (= {:type :attractor :x 100 :y 200 :radius 60 :mass 360}
           (first (:attractors w1))))))

(deftest the-ship-wraps-instead-of-bouncing
  (let [w1 (w/step (world :ship {:x 1 :vx -10}) 1.0)]
    (is (= -10.0 (:vx (:ship w1))) "no bounce: the velocity survives the edge")
    (is (= 791.0 (:x (:ship w1))) "re-enters from the opposite side")))

(deftest restart-rebuilds-the-round
  (let [w1 (w/restart (world :prizes [{:x 400 :y 300 :radius 40}]) 5
                      (let [s (volatile! 7)]
                        (fn [] (vswap! s #(rem (inc (* 1103515245 %)) 2147483648)))))]
    (is (= 5 (:total-prizes w1)))
    (is (= 5 (count (:prizes w1))))
    (is (zero? (:score w1)))
    (is (= :build (:phase w1)))))

(defn- build-world
  [& {:keys [attractors]}]
  (assoc (world :attractors (or attractors [])) :phase :build))

(deftest step-parks-the-ship-during-build
  (let [w0 (build-world :attractors [{:x 0 :y 0 :mass 500}])
        w1 (w/step w0 1.0)]
    (is (= w0 w1) "nothing moves until the ship is launched")))

(deftest launch-needs-a-well
  (let [w1 (w/launch (build-world))]
    (is (= :build (:phase w1)) "nothing to ride: keep building")))

(deftest launch-starts-the-flight
  (let [w0 (build-world :attractors [{:x 200 :y 150 :radius 60 :mass 360}])
        w1 (w/launch w0)]
    (is (= :playing (:phase w1)))
    (is (= (:ship w0) (:ship w1)) "launching releases the ship, does not move it")
    (is (= (:attractors w0) (:attractors w1)))))

(deftest launch-outside-build-changes-nothing
  (let [w (assoc (world :prizes [{:x 100 :y 100 :radius 20}]) :phase :playing)]
    (is (= w (w/launch w)))
    (is (= (assoc w :phase :won) (w/launch (assoc w :phase :won))))))

(deftest a-launched-ship-rides-the-field
  (let [w0 (assoc (w/launch (build-world
                             :attractors [{:x 250 :y 300 :radius 100 :mass 1000}]))
                  :ship {:x 400 :y 300 :vx 0 :vy 0 :radius 10})
        ;; 60 ticks: one second of flight. The size-scaled well is strong
        ;; enough to slingshot the ship through itself, so assert the
        ;; phase-robust invariants: real displacement, real speed.
        w60 (nth (iterate #(w/step % 1.0) w0) 60)]
    (is (< (Math/abs (- 250 (:x (:ship w60)))) 150)
        "a second of size-scaled pull brings the ship net-closer to the well")
    (is (> (Math/abs (:vx (:ship w60))) 1.0)
        "a second of pull is a real speed, not epsilon")))
