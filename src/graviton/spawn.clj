(ns graviton.spawn
  "Entity creation and placement. All randomness flows through an injected
  rng fn (zero-arg, returning the next number in [0, 2147483648)), so every
  test and replay is deterministic. Pure — no rendering or IO.")

(def RADIUS-CAP 100)

(defn- rand-range
  "rng returns the next integer in [0, 2147483648); scale it into [lo, hi)."
  [rng lo hi]
  (+ lo (* (/ (rng) 2147483648.0) (- hi lo))))

(defn- overlaps?
  [{:keys [x y radius]} e2]
  (< (Math/hypot (- x (:x e2)) (- y (:y e2)))
     (+ radius (:radius e2))))

(defn- place
  "One non-overlapping disk in [0,w)x[0,h), inset by its radius, avoiding
  :existing. Retries until it fits or gives up; nil when it never does."
  [w h {:keys [min-r max-r existing retries]} rng]
  (let [min-r (or min-r 1)
        max-r (or max-r min-r)
        r (rand-range rng min-r (inc (max min-r max-r)))
        inset (+ r 1)
        x (rand-range rng inset (- w inset))
        y (rand-range rng inset (- h inset))
        disk {:x x :y y :radius r}]
    (if (some (partial overlaps? disk) existing)
      (when (pos? (or retries 0))
        (place w h {:min-r min-r :max-r max-r :existing existing
                    :retries (dec (or retries 0))} rng))
      disk)))

(defn disks
  "n non-overlapping disks, each clear of :existing."
  [n w h opts rng]
  (let [opts (merge {:retries 50} opts)]
    (loop [n n
           placed []
           existing (:existing opts)]
      (if (zero? n)
        placed
        (if-let [d (place w h (assoc opts :existing existing) rng)]
          (recur (dec n) (conj placed d) (conj existing d))
          placed)))))

(defn prizes
  "Place the round's n prizes on state's screen, apart from each other and
  from :existing. Returns the world map the game step reads: :prizes,
  :total-prizes."
  [{:keys [width height rng existing] :as state} n]
  (let [ps (disks n width height {:min-r 40 :max-r 50 :existing (or existing [])} rng)]
    {:prizes ps :total-prizes n}))

(defn attractor
  "An attractor from a drag: position where the drag began, strength from
  its length (capped)."
  [state x y drag-len]
  (let [radius (min RADIUS-CAP drag-len)
        mass (/ (* radius radius) 10)]
    {:type :attractor :x x :y y
     :radius radius :mass mass}))

(defn maybe-deathzone
  "The gravity-well tax: once (mod attractor-count 4) > 2, a death zone
  appears somewhere on screen. Returns the world map with :deathzones (a
  seq, possibly empty)."
  [{:keys [width height rng attractors existing] :as state}]
  (let [n (count attractors)]
    (if (> (mod n 4) 2)
      (let [zone (place width height {:min-r 20 :max-r 30
                                      :existing (concat attractors existing)
                                      :retries 50} rng)]
        (when zone
          {:deathzones [(assoc zone :type :deathzone)]}))
      {:deathzones []})))
