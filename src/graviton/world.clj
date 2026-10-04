(ns graviton.world
  "The game state and its step. A round: drag out gravity wells to steer
  the ship to every prize; avoid the death zones that pile up when you
  place too many wells. Pure — the shell owns IO and the clock."
  (:require [graviton.physics :as phys]
            [graviton.spawn :as spawn]))

(def TOTAL-PRIZES 5)

(defn step
  "One tick of flight: move the ship under the superposed field, collect
   overlapped prizes, then resolve the round's phase (:playing, :won,
   :lost). During :build the ship is parked; :won and :lost are terminal --
   the round freezes there until restart."
  [{:keys [width height ship attractors prizes deathzones score] :as world} dt]
  (if (contains? #{:build :won :lost} (:phase world))
    world
    (let [[ax ay] (phys/accel-at attractors (:x ship) (:y ship))
          ship' (phys/integrate ship ax ay dt width height)
          hit? (fn [e] (phys/collides? ship' e))
          hit-prizes (filter hit? prizes)
          remaining (vec (remove (set hit-prizes) prizes))
          score' (+ (or score 0) (count hit-prizes))
          zone-hit? (boolean (some hit? deathzones))]
      (assoc world
             :ship ship'
             :prizes remaining
             :score score'
             :phase (cond
                      zone-hit? :lost
                      (and (seq prizes) (empty? remaining) (= score' (:total-prizes world))) :won
                      :else :playing)))))

(defn launch
  "End the build phase and release the ship. Needs at least one well to
  ride; outside :build it changes nothing."
  [world]
  (if (and (= :build (:phase world)) (seq (:attractors world)))
    (assoc world :phase :playing)
    world))

(defn add-attractor
  "Fold a drag-created attractor into the world: append it, and add its
  death zone if the well count crosses the threshold."
  [world x y drag-len]
  (let [a (spawn/attractor world x y drag-len)
        attractors (conj (vec (:attractors world)) a)
        world' (assoc world :attractors attractors)]
    (if-let [zone (seq (:deathzones (spawn/maybe-deathzone
                                      (assoc world' :attractors attractors))))]
      (update world' :deathzones (fnil into []) zone)
      world')))

(defn restart
  "A fresh round: n prizes from the injected rng, ship centred and at rest.
  The rng is kept in the world -- add-attractor needs it when a death zone
  spawns."
  [world n rng]
  (let [{:keys [prizes total-prizes]} (spawn/prizes (assoc world :rng rng) n)]
    (assoc (select-keys world [:width :height])
           :rng rng
           :prizes prizes
           :total-prizes (or total-prizes n)
           :score 0
           :phase :build
           :attractors []
           :deathzones []
           :ship {:x (/ (:width world) 2.0)
                  :y (/ (:height world) 2.0)
                  :vx 0 :vy 0 :radius 10})))

(defn initial
  "The world the shell boots into."
  [width height rng]
  (restart {:width width :height height} TOTAL-PRIZES rng))
