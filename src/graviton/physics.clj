(ns graviton.physics
  "Gravity-well physics. The field is a list of attractors; each well's
  pull is capped at MAX-ACCEL before the superposed sum, so no single
  nearby well can drown out the others. Pure math, no rendering or IO.")

(def MAX-ACCEL 1.0)

(defn nearest-point
  "Snap p to the nearest site of the lattice offset by radius (spacing 2r),
  the sampling grid the field was drawn on."
  [radius p]
  (- p (- (mod p (* 2 radius)) radius)))

(defn attractor-accel
  "The acceleration [ax ay] an attractor exerts at (x, y): inverse-square in
  distance and scaled by the well's size -- mass times radius -- so a
  bigger source genuinely pulls harder."
  [{:keys [x y mass radius]} px py]
  (let [dx (- x px)
        dy (- y py)
        r2 (+ (* dx dx) (* dy dy) 1e-7)
        strength (/ (* mass radius) r2)
        dist (Math/sqrt r2)]
    [(* strength (/ dx dist)) (* strength (/ dy dist))]))

(defn accel-at
  "Superposed acceleration [ax ay] at (x, y) over all attractors. Each
  well's pull is capped at MAX-ACCEL first (direction preserved), then the
  capped pulls are summed uncapped: one nearby well contributes at most
  one MAX-ACCEL, so other wells always keep a vote."
  [attractors px py]
  (reduce (fn [[sx sy] a]
            (let [[ax ay] (attractor-accel a px py)
                  mag (Math/sqrt (+ (* ax ax) (* ay ay)))]
              (if (> mag MAX-ACCEL)
                (let [k (/ MAX-ACCEL mag)]
                  [(+ sx (* ax k)) (+ sy (* ay k))])
                [(+ sx ax) (+ sy ay)])))
          [0.0 0.0]
          attractors))

(defn integrate
  "Step a body one dt: velocity gains acceleration, position gains velocity
  (semi-implicit Euler). The court wraps: a body leaving any edge re-enters
  from the opposite side with its velocity untouched. The sides must be
  positive -- wrapping divides by them (the writ World refine enforces
  this for game states)."
  [{:keys [x y vx vy] :as body} ax ay dt width height]
  (let [vx' (max -20 (min 20 (+ vx (* ax dt))))
        vy' (max -10 (min 10 (+ vy (* ay dt))))]
    (assoc body
           :x (mod (+ x (* vx' dt)) width)
           :y (mod (+ y (* vy' dt)) height)
           :vx vx'
           :vy vy')))

(defn distance
  [x1 y1 x2 y2]
  (Math/sqrt (+ (* (- x1 x2) (- x1 x2)) (* (- y1 y2) (- y1 y2)))))

(defn collides?
  "True when two circle-shaped entities overlap."
  [e1 e2]
  (< (distance (:x e1) (:y e1) (:x e2) (:y e2))
     (+ (:radius e1) (:radius e2))))
