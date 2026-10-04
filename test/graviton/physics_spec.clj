(ns graviton.physics-spec
  "The contract for graviton.physics: gravity wells pull inverse-square
  toward themselves with strength scaled by their size (mass*radius), each
  well's pull is capped at MAX-ACCEL before the superposed sum (so one
  nearby well cannot monopolize the field), and integrate steps a body by
  semi-implicit Euler across a court that wraps at every edge."
  (:require [graviton.physics :refer [MAX-ACCEL]]
            [writ.spec :refer [spec ann law graph refine assume]]))

(spec graviton.physics {:test {:seed 42}})

;; the code may call these pure members (writ rejects host calls otherwise)
(assume Math/sqrt  [Double -> Double])
(assume Math/abs   [Double -> Double])

(defn magnitude
  [[ax ay]]
  (Math/sqrt (+ (* ax ax) (* ay ay))))

(defn near
  [a b]
  (< (Math/abs (- a b)) 1e-6))

;; wells bounded to the game's domain (radius capped at 100 in play, mass
;; r^2/10); unbounded generators make the 1e-7 r-squared epsilon exceed the
;; absolute near tolerance
(refine Well [a {:x Double, :y Double, :mass Nat, :radius Double}]
  (and (pos? (:mass a)) (pos? (:radius a))
       (<= (:mass a) 100000) (<= (:radius a) 1000.0)))

(refine Body [b {:x Double, :y Double, :vx Double, :vy Double}] true)
(refine Disk [d {:x Double, :y Double, :vx Double, :vy Double, :radius (Opt Double)}] true)

(ann nearest-point   [Int Int -> Int])
(ann attractor-accel [Well Double Double -> (Tuple Double Double)])
(ann accel-at        [(List Well) Double Double -> (Tuple Double Double)])
(ann integrate       [Body Double Double Double Double Double -> Body])
(ann distance        [Double Double Double Double -> Double])
(ann collides?       [Disk Disk -> Bool])

(graph motion
  {:start  [:body {:x 400.0 :y 300.0 :vx 0.0 :vy 0.0}]
   :states {:body {:x Double, :y Double, :vx Double, :vy Double}}
   :edges  {:body {[integrate Double Double Double Double Double] #{:body}}}})

;; --- nearest-point -----------------------------------------------------------------

(law nearest-point-snaps-to-the-offset-lattice
  (forall [p Nat]
    (= (nearest-point 20 p) (+ (* 40 (quot p 40)) 20))))

;; --- attractor-accel ---------------------------------------------------------------

(law pull-points-at-the-well
  (forall [a Well, px Double, py Double]
    (or (and (<= (:x a) px) (<= (first (attractor-accel a px py)) 0))
        (and (>= (:x a) px) (>= (first (attractor-accel a px py)) 0)))))

(law pull-magnitude-is-mass-times-radius-over-r-squared
  (forall [a Well]
    (=> (and (<= -1000 (:x a) 1000) (<= -1000 (:y a) 1000))
         (near (magnitude (attractor-accel a (+ (:x a) 100) (:y a)))
               (/ (* (:mass a) (:radius a)) 10000.0)))))

;; --- accel-at ----------------------------------------------------------------------

(law no-wells-no-pull
  (= (accel-at [] 55.0 66.0) [0.0 0.0]))

(law superposition-holds-when-no-well-clamps
  (forall [a Well, b Well, px Double, py Double]
    (=> (and (<= (magnitude (attractor-accel a px py)) MAX-ACCEL)
             (<= (magnitude (attractor-accel b px py)) MAX-ACCEL)
             (<= (let [[ax ay] (attractor-accel a px py)
                       [bx by] (attractor-accel b px py)]
                   (magnitude [(+ ax bx) (+ ay by)]))
                 MAX-ACCEL))
         (= (accel-at [a b] px py)
            (let [[ax ay] (attractor-accel a px py)
                  [bx by] (attractor-accel b px py)]
              [(+ ax bx) (+ ay by)])))))

(law a-close-monster-cannot-monopolize-the-field
  (pos? (first (accel-at [{:x 0.0 :y 0.0 :mass 10000 :radius 100.0}
                          {:x 30.0 :y 0.0 :mass 120 :radius 2.0}
                          {:x 30.0 :y 0.0 :mass 120 :radius 2.0}]
                         10.0 0.0))))

(law the-clamp-bites-exactly
  (forall [a Well, px Double, py Double]
    (=> (> (magnitude (attractor-accel a px py)) MAX-ACCEL)
        (near (magnitude (accel-at [a] px py)) MAX-ACCEL))))

;; --- distance / collides? ------------------------------------------------------------

(law distance-is-symmetric
  (forall [x1 Double, y1 Double, x2 Double, y2 Double]
    (= (distance x1 y1 x2 y2) (distance x2 y2 x1 y1))))

(law distance-of-a-3-4-5-triangle
  (near (distance 3.0 4.0 0.0 0.0) 5.0))

(law collides-is-touching-circles
  (forall [e1 Body, e2 Body]
    (= (collides? (assoc e1 :radius 10) (assoc e2 :radius 10))
       (< (distance (:x e1) (:y e1) (:x e2) (:y e2)) 20))))

;; --- integrate -----------------------------------------------------------------------

(defn scale
  "A Nat in [0 50] mapped onto [lo hi]."
  [n lo hi]
  (+ lo (* (/ (double n) 50.0) (- hi lo))))

(defn drifting
  "A body well inside an 800x600 screen with velocity small enough that
  one step of dt never reaches a wall or a velocity cap, from four Nats."
  [nx ny nvx nvy]
  {:x (scale nx 100.0 700.0) :y (scale ny 100.0 500.0)
   :vx (scale nvx -15.0 15.0) :vy (scale nvy -9.0 9.0)})

(law drift-advances-position-and-keeps-velocity
  (forall [nx Nat, ny Nat, nvx Nat, nvy Nat, nd Nat]
    (let [b (drifting nx ny nvx nvy)
          dt (/ (double (inc nd)) 30.0)]
      (and (near (:x (integrate b 0.0 0.0 dt 800 600)) (+ (:x b) (* (:vx b) dt)))
           (near (:y (integrate b 0.0 0.0 dt 800 600)) (+ (:y b) (* (:vy b) dt)))
           (near (:vx (integrate b 0.0 0.0 dt 800 600)) (:vx b))
           (near (:vy (integrate b 0.0 0.0 dt 800 600)) (:vy b))))))

(law wrapping-the-right-edge-preserves-velocity
  (and (near (:x (integrate {:x 795.0 :y 300.0 :vx 20.0 :vy 0.0} 0.0 0.0 1.0 800 600)) 15.0)
       (near (:vx (integrate {:x 795.0 :y 300.0 :vx 20.0 :vy 0.0} 0.0 0.0 1.0 800 600)) 20.0)))

(law acceleration-adds-to-velocity
  (forall [nx Nat, ny Nat, nvx Nat, nvy Nat, nd Nat]
    (let [b (drifting nx ny nvx nvy)
          dt (/ (double (inc nd)) 30.0)]
      (and (near (:vx (integrate b 0.5 0.0 dt 800 600)) (+ (:vx b) (* 0.5 dt)))
           (near (:vy (integrate b 0.0 -0.25 dt 800 600)) (+ (:vy b) (* -0.25 dt)))))))

(law drift-follows-any-court-any-acceleration
  (forall [nx Nat, ny Nat, nvx Nat, nvy Nat, nd Nat, nax Nat, nay Nat, nw Nat, nh Nat]
    (let [width  (scale nw 100.0 1000.0)
          height (scale nh 100.0 1000.0)
          b {:x (* width (scale nx 0.2 0.8)) :y (* height (scale ny 0.2 0.8))
             :vx (scale nvx -15.0 15.0) :vy (scale nvy -9.0 9.0)}
          dt (/ (double (inc nd)) 60.0)
          ax (scale nax -0.5 0.5)
          ay (scale nay -0.25 0.25)
          s (integrate b ax ay dt width height)]
      ;; no wall or velocity cap is in reach, so pure Euler holds
      (and (near (:vx s) (+ (:vx b) (* ax dt)))
           (near (:vy s) (+ (:vy b) (* ay dt)))
           (near (:x s) (+ (:x b) (* (:vx s) dt)))
           (near (:y s) (+ (:y b) (* (:vy s) dt)))))))

(law wrapping-the-left-edge
  (near (:x (integrate {:x 5.0 :y 300.0 :vx -20.0 :vy 0.0} 0.0 0.0 1.0 800 600)) 785.0))

(law wrapping-the-top-edge
  (near (:y (integrate {:x 400.0 :y 5.0 :vx 0.0 :vy -10.0} 0.0 0.0 1.0 800 600)) 595.0))

(law wrapping-the-bottom-edge
  (near (:y (integrate {:x 400.0 :y 595.0 :vx 0.0 :vy 10.0} 0.0 0.0 1.0 800 600)) 5.0))

(law velocity-clamps-at-the-cap
  (near (:vx (integrate {:x 400.0 :y 300.0 :vx 19.0 :vy 0.0} 10.0 0.0 1.0 800 600))
        20.0))

(law nearest-point-is-the-nearest-site-of-its-lattice
  (forall [r Nat, p Nat]
    (=> (pos? r)
        (= (nearest-point r p) (- (+ p r) (mod p (* 2 r)))))))
