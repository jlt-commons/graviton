(ns graviton.render-test
  "Pure render decisions: the field-vector tint and the ship's triangle
  geometry. The draw calls themselves are verified by the headless smoke
  run; here only the math is under test."
  (:require [clojure.test :refer [deftest is]]
            [graviton.render :as render]))

(defn- red-channel
  [packed]
  (bit-and packed 0xff))

(deftest field-vector-red-follows-the-sigmoid
  (let [r0 (red-channel (render/field-vector-color 0.0))
        r5 (red-channel (render/field-vector-color 5.0))
        r20 (red-channel (render/field-vector-color 20.0))]
    ;; 0x1cb*sigmoid(0) - 0xcc = round(459*0.5) - 204 = 26
    (is (= 26 r0) "no pull: dim red, not the 204 floor")
    (is (= 255 r20) "strong pull: full red")
    (is (< r0 r5 r20) "red grows with magnitude")))

(deftest field-vector-green-follows-the-sigmoid
  (let [g (fn [mag] (bit-and (bit-shift-right (render/field-vector-color mag) 8) 0xff))]
    (is (= 128 (g 0.0)) "no pull: half green")
    ;; greenness 100: green saturates only for mag in the hundreds
    (is (= 255 (g 1000.0)) "very strong pull: full green")
    (is (< (g 0.0) (g 1000.0)) "green grows with magnitude")))

(defn- cross
  "Z of the cross product (p2-p1)x(p3-p1): its sign is the winding."
  [[x1 y1] [x2 y2] [x3 y3]]
  (- (* (- x2 x1) (- y3 y1)) (* (- y2 y1) (- x3 x1))))

(defn- cos-sim
  [u v]
  (let [dot (+ (* (nth u 0) (nth v 0)) (* (nth u 1) (nth v 1)))
        nu (Math/sqrt (+ (* (nth u 0) (nth u 0)) (* (nth u 1) (nth u 1))))
        nv (Math/sqrt (+ (* (nth v 0) (nth v 0)) (* (nth v 1) (nth v 1))))]
    (/ dot (* nu nv))))

(deftest ship-nose-points-along-velocity
  (doseq [[vx vy] [[20 0] [10 10] [0 -15] [-7 3]]]
    (let [ship {:x 400 :y 300 :vx vx :vy vy :radius 10}
          [[nx ny] _ _] (render/ship-vertices ship)
          sim (cos-sim [(- nx 400) (- ny 300)] [vx vy])]
      (is (> sim 0.999) (str "nose tracks velocity " vx "," vy)))))

(deftest a-resting-ship-points-right
  (let [[[nx ny] _ _] (render/ship-vertices {:x 400 :y 300 :vx 0 :vy 0 :radius 10})]
    (is (> nx 400) "nose forward")
    (is (= 300.0 ny) "level")))

(deftest ship-triangle-winding-survives-rotation
  ;; rlgl immediate-mode triangles are backface-culled; clockwise winding
  ;; in y-down screen coords (negative cross) is what renders
  (doseq [[vx vy] [[20 0] [10 10] [0 -15]]]
    (let [[p1 p2 p3] (render/ship-vertices {:x 400 :y 300 :vx vx :vy vy :radius 10})]
      (is (neg? (cross p1 p2 p3)) (str "clockwise winding at " vx "," vy)))))
