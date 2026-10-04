(ns graviton.world-spec
  "The contract for graviton.world: a round steps a ship through a
  gravity-well field, collects the prizes it touches, and ends :won when
  every prize is collected or :lost when the ship enters a death zone."
  (:require [graviton.physics :as phys]
            [graviton.world :refer [TOTAL-PRIZES]]
            [writ.spec :refer [spec ann law graph refine assume]]))

(spec graviton.world {:test {:seed 42}})

;; the world builds on graviton.physics, which its own spec covers; here it
;; is a given, typed for the calls and tested where the world calls it
(assume phys/collides? [{:x Double, :y Double, :radius (Opt Double)}
                        {:x Double, :y Double, :radius (Opt Double)}
                        -> Bool])
(assume phys/accel-at [(List {:x Double, :y Double, :radius Double, :mass Double})
                       Double Double -> (Tuple Double Double)])
(assume phys/integrate [{:x Double, :y Double, :vx Double, :vy Double}
                        Double Double Double Double Double
                        -> {:x Double, :y Double, :vx Double, :vy Double}])

(refine World [w {:width Nat, :height Nat,
                   :ship {:x Double, :y Double, :vx Double, :vy Double,
                          :radius Double},
                   :prizes (List {:x Double, :y Double, :radius Double}),
                   :deathzones (List {:x Double, :y Double, :radius Double}),
                   :attractors (List {:x Double, :y Double, :radius Double,
                                      :mass Double, :type Keyword}),
                   :score Nat, :total-prizes Nat, :phase Keyword}]
  ;; integrate wraps via mod, so the court must have positive sides; and a
  ;; legal round always has the ship inside the court (restart centres it,
  ;; wrapping keeps it in) -- generated out-of-court ships would teleport
  ;; on their first step and desync the pre-step collision laws
  (and (pos? (:width w)) (pos? (:height w))
       (>= (:x (:ship w)) 0) (< (:x (:ship w)) (:width w))
       (>= (:y (:ship w)) 0) (< (:y (:ship w)) (:height w))))

(defn settle-playing
  "Any World, made one that plays. The base's score picks the shape, so a
  generated :playing state is sometimes one prize from a win, sometimes a
  fresh round, sometimes a step from a zone."
  [w]
  (case (mod (or (:score w) 0) 6)
    0 {:width 800, :height 600,
       :ship {:x 400.0, :y 300.0, :vx 1.0, :vy 0.5, :radius 12.0},
       :prizes [{:x 405.0, :y 303.0, :radius 10.0}],
       :deathzones [], :attractors [],
       :score 0, :total-prizes 1, :phase :playing}
    3 {:width 800, :height 600,
       :ship {:x 400.0, :y 300.0, :vx 0.0, :vy 0.0, :radius 12.0},
       :prizes [{:x 404.0, :y 302.0, :radius 10.0}],
       :deathzones [], :attractors [],
       :score 1, :total-prizes 2, :phase :playing}
    2 {:width 800, :height 600,
       :ship {:x 400.0, :y 300.0, :vx 2.0, :vy 1.0, :radius 12.0},
       :prizes [{:x 100.0, :y 100.0, :radius 10.0}
                {:x 700.0, :y 500.0, :radius 10.0}],
       :deathzones [{:x 402.0, :y 301.0, :radius 10.0}],
       :attractors [], :score 0, :total-prizes 2, :phase :playing}
    {:width 800, :height 600,
     :ship {:x 400.0, :y 300.0, :vx -1.0, :vy 0.0, :radius 12.0},
     :prizes [{:x 120.0, :y 120.0, :radius 10.0}
              {:x 680.0, :y 480.0, :radius 10.0}
              {:x 400.0, :y 100.0, :radius 10.0}],
     :deathzones [], :attractors [],
     :score 0, :total-prizes 3, :phase :playing}))

(defn settle-build
  "Any World, made one that builds: parked ship, sometimes with a well
  down and sometimes with none."
  [w]
  (let [base {:width 800, :height 600,
              :ship {:x 400.0, :y 300.0, :vx 0.0, :vy 0.0, :radius 10.0},
              :prizes [{:x 120.0, :y 120.0, :radius 10.0}],
              :deathzones [], :attractors [],
              :score 0, :total-prizes 1, :phase :build}]
    (if (zero? (mod (or (:score w) 0) 2))
      base
      (assoc base :attractors [{:x 250.0, :y 300.0, :radius 60.0,
                                :mass 360.0, :type :attractor}]))))

(refine Playing [w World] (= :playing (:phase w)) {:build settle-playing})
(refine Build   [w World] (= :build (:phase w)) {:build settle-build})

(defn settle-won
  "Any World, made a finished round that won."
  [w]
  {:width 800, :height 600,
   :ship {:x 400.0, :y 300.0, :vx 0.0, :vy 0.0, :radius 12.0},
   :prizes [], :deathzones [], :attractors [],
   :score 1, :total-prizes 1, :phase :won})

(defn settle-lost
  "Any World, made a finished round that lost."
  [w]
  {:width 800, :height 600,
   :ship {:x 402.0, :y 301.0, :vx 1.0, :vy 0.0, :radius 12.0},
   :prizes [{:x 120.0, :y 120.0, :radius 10.0}],
   :deathzones [{:x 400.0, :y 300.0, :radius 15.0}], :attractors [],
   :score 0, :total-prizes 2, :phase :lost})

(refine Won     [w World] (= :won (:phase w)) {:build settle-won})
(refine Lost    [w World] (= :lost (:phase w)) {:build settle-lost})

(ann step          [World Double -> World])
(ann launch        [World -> World])
(ann add-attractor [World Double Double Double -> World])
(ann restart       [{:width Nat, :height Nat} Nat (-> Nat) -> World])
(ann initial       [Nat Nat (-> Nat) -> World])

(graph round
  {:start  [:build {:width 800, :height 600,
                    :ship {:x 400.0, :y 300.0, :vx 0.0, :vy 0.0, :radius 10.0},
                    :prizes (), :deathzones (), :attractors (),
                    :score 0, :total-prizes 0, :phase :build}]
   :states {:build Build, :playing Playing, :won Won, :lost Lost}
   :edges  {:build   {[add-attractor Double Double Double] #{:build}
                      [launch] #{:build :playing}}
            :playing {[step Double] #{:playing :won :lost}}}
   :before [[:playing :won] [:playing :lost]]
   :final  [:won :lost]})

(defn dim
  "A screen side in [400 2000] from a Nat."
  [n]
  (+ 400 (* 32 (min n 50))))

(defn touched
  "The prizes of w that the ship touches, by where they sit."
  [w]
  (let [s (:ship w)]
    (filter #(phys/collides? s %) (:prizes w))))

(defn zone-hit
  "True when the ship of w (as stepped, if it moves) touches a zone."
  [w]
  (boolean (some #(phys/collides? (:ship w) %) (:deathzones w))))

(defn remaining
  "The prizes of w the ship does not touch."
  [w]
  (let [s (:ship w)]
    (remove #(phys/collides? s %) (:prizes w))))

;; --- step: the phase follows the collisions ---------------------------------------

(law a-zone-the-ship-touches-loses-the-round
  (forall [w Playing]
    (=> (zone-hit w)
        (= :lost (:phase (step w 0.5))))))

(law collecting-the-last-prize-wins-the-round
  (forall [w Playing]
    (=> (and (not (zone-hit w))
             (= (+ (:score w) (count (touched w))) (:total-prizes w)))
        (= :won (:phase (step w 0.5))))))

(law prizes-still-out-there-play-on
  (forall [w Playing]
    (=> (and (not (zone-hit w))
             (< (+ (:score w) (count (touched w))) (:total-prizes w))
             (seq (remaining w)))
        (= :playing (:phase (step w 0.5))))))

;; --- step: bookkeeping --------------------------------------------------------------

(law the-prizes-that-survive-are-the-ones-not-touched
  (forall [w Playing]
    (=> (not (zone-hit w))
        (= (set (map (juxt :x :y) (remaining w)))
           (set (map (juxt :x :y) (:prizes (step w 0.5))))))))

(law the-score-counts-what-was-touched
  (forall [w Playing]
    (=> (not (zone-hit w))
        (= (:score (step w 0.5))
           (+ (:score w) (count (touched w)))))))

;; --- add-attractor -------------------------------------------------------------------

(law add-attractor-grows-the-well-count-by-one
  (forall [w Build, x Double, y Double, d Double]
    (= (inc (count (:attractors w)))
       (count (:attractors (add-attractor w x y d))))))

;; --- launch --------------------------------------------------------------------------

(law a-well-less-build-will-not-launch
  (forall [w Build]
    (=> (empty? (:attractors w))
        (= :build (:phase (launch w))))))

(law launching-a-built-round-releases-the-ship
  (forall [w Build]
    (=> (seq (:attractors w))
        (and (= :playing (:phase (launch w)))
             (= (:ship w) (:ship (launch w)))
             (= (:attractors w) (:attractors (launch w)))))))

(law the-build-phase-parks-the-ship
  (forall [w Build, dt Double]
    (= w (step w dt))))

(law the-round-freezes-once-lost
  (forall [w Lost, dt Double]
    (= w (step w dt))))

(law the-round-freezes-once-won
  (forall [w Won, dt Double]
    (= w (step w dt))))

(law the-new-well-sits-where-the-drag-began
  (forall [w Playing, x Double, y Double, d Double]
    (let [a (peek (vec (:attractors (add-attractor w x y d))))]
      (and (= x (:x a)) (= y (:y a))))))

;; --- restart / initial ----------------------------------------------------------------

(law restart-clears-the-round
  (forall [w World, n Nat, f (-> Nat)]
    (let [r (restart w n f)]
      (and (= :build (:phase r))
           (zero? (:score r))
           (= n (:total-prizes r))
           (empty? (:attractors r))
           (empty? (:deathzones r))))))

(law initial-is-a-fresh-centred-round
  (forall [n Nat, m Nat, f (-> Nat)]
    (let [w (initial (dim n) (dim m) f)]
      (and (= TOTAL-PRIZES (:total-prizes w))
           (= :build (:phase w))
           (zero? (:score w))
           (= (:x (:ship w)) (/ (dim n) 2.0))
           (= (:y (:ship w)) (/ (dim m) 2.0))
           (= (dim n) (:width w))
           (= (dim m) (:height w))))))
