(ns graviton.render
  "World -> raylib 2D draw calls. Pure decision logic over the world plus
  the draw side effects; the vector field, wells, prizes, zones and ship
  each get their layer."
  (:require [graviton.input :as gin]
            [graviton.physics :as phys]
            [graviton.raylib :as rl]))

(def FIELD-SPACING 40)
(def FIELD-VEC-LEN 6.0)
(def FIELD-VEC-SCALE 100.0)

(defn field-vector-color
  "The original's sigmoid-tinted red/green field vector color, from the
  acceleration magnitude at a lattice point."
  [mag]
  (let [sigmoid #(/ 1.0 (+ 1.0 (Math/exp (- %))))
        redness 1.0
        greenness 100.0
        r (max 0 (- (Math/round (* 0x1cb (sigmoid (* mag redness)))) 0xcc))
        g (max 0 (Math/round (* 0xff (sigmoid (/ mag greenness)))))
        b 0]
    (rl/rgba (min 255 r) (min 255 g) b 255)))

(defn draw-vector-field!
  "Sample the superposed field on the nearest-point lattice and draw a
  short vector at each site, tinted by the local pull."
  [world]
  (let [{:keys [width height attractors]} world]
    (doseq [x (range FIELD-SPACING width FIELD-SPACING)
            y (range FIELD-SPACING height FIELD-SPACING)]
      (let [[ax ay] (phys/accel-at attractors x y)
            mag (Math/sqrt (+ (* ax ax) (* ay ay)))]
        (when (> mag 1e-3)
          (let [len (* FIELD-VEC-LEN (min 1.0 (* 10.0 mag)))
                norm (if (pos? mag) (/ len mag) 0.0)]
            (rl/line! :x1 x :y1 y
                      :x2 (+ x (* norm ax)) :y2 (+ y (* norm ay))
                      :color (rl/fade (field-vector-color mag) 0.5))))))))

(defn draw-attractors!
  [{:keys [attractors]}]
  (doseq [{:keys [x y radius]} attractors]
    (rl/circle-lines! :x x :y y :radius radius :color (rl/fade rl/WHITE 0.35))
    (rl/circle! :x x :y y :radius 3 :color rl/YELLOW)))

(defn draw-prizes!
  [{:keys [prizes]}]
  (doseq [{:keys [x y radius]} prizes]
    (rl/circle! :x x :y y :radius radius
                :color (rl/fade (rl/rgba 0 255 100 255) 0.7))
    (rl/circle-lines! :x x :y y :radius radius
                      :color (rl/fade (rl/rgba 0 100 255 255) 0.7))))

(defn draw-deathzones!
  [{:keys [deathzones]}]
  (doseq [{:keys [x y radius]} deathzones]
    (rl/circle! :x x :y y :radius radius
                :color (rl/fade rl/RED 0.35))
    (rl/circle-lines! :x x :y y :radius radius :color rl/MAROON)))

(defn ship-vertices
  "The ship's triangle in world coords: a long nose pointing along the
  velocity, two swept wings back, scaled by radius. Winding is clockwise
  in y-down screen coords -- rlgl immediate-mode triangles are
  backface-culled. atan2 0 0 is undefined on jolt; at rest the ship
  points right."
  [{:keys [x y vx vy radius]}]
  (let [heading (if (and (zero? vx) (zero? vy)) 0.0 (Math/atan2 vy vx))
        cos-h (Math/cos heading)
        sin-h (Math/sin heading)
        verts [[1.6 0.0] [-1.0 -0.9] [-1.0 0.9]]]
    (map (fn [[sx sy]]
           [(+ x (* radius (- (* sx cos-h) (* sy sin-h))))
            (+ y (* radius (+ (* sx sin-h) (* sy cos-h))))])
         verts)))

(defn draw-ship!
  "A triangle pointing along the velocity."
  [{:keys [ship]}]
  (let [[p1 p2 p3] (ship-vertices ship)]
    (rl/triangle! :p1 p1 :p2 p2 :p3 p3 :color rl/SKYBLUE)
    (rl/line! :x1 (nth p1 0) :y1 (nth p1 1) :x2 (nth p2 0) :y2 (nth p2 1)
              :color rl/DARKBLUE)
    (rl/line! :x1 (nth p2 0) :y1 (nth p2 1) :x2 (nth p3 0) :y2 (nth p3 1)
              :color rl/DARKBLUE)
    (rl/line! :x1 (nth p3 0) :y1 (nth p3 1) :x2 (nth p1 0) :y2 (nth p1 1)
              :color rl/DARKBLUE)))

(defn draw-drag!
  "The in-progress drag: a line from its origin to the mouse."
  [snap drag]
  (when drag
    (rl/line! :x1 (:down-x drag) :y1 (:down-y drag)
              :x2 (:mx snap) :y2 (:my snap)
              :color (rl/fade rl/GOLD 0.8))
    (rl/circle-lines! :x (:down-x drag) :y (:down-y drag)
                      :radius (max 1.0 (gin/drag-length snap drag))
                      :color (rl/fade rl/GOLD 0.4))))

(defn draw-hud!
  [{:keys [score total-prizes phase attractors width height]}]
  (rl/text! (str "Score: " score "/" total-prizes) :x 10 :y 10 :size 22
            :color rl/RAYWHITE)
  (case phase
    :build (let [msg (if (seq attractors)
                        "SPACE to launch the ship"
                        "Drag to place a gravity well, then SPACE to launch")]
              (rl/text! msg :x 10 :y 36 :size 20 :color (rl/fade rl/GOLD 0.9)))
    :won (let [msg "You Win! - press R to play again"
               tw (rl/text-width msg :size 30)]
           (rl/text! msg :x (/ (- width tw) 2) :y (/ (- height 30) 2) :size 30
                     :color rl/GOLD))
    :lost (let [msg "You Lost - press R to play again"
                tw (rl/text-width msg :size 30)]
            (rl/text! msg :x (/ (- width tw) 2) :y (/ (- height 30) 2) :size 30
                      :color rl/RED))
    nil))

(defn draw-world!
  "Everything for one frame: field, wells, zones, prizes, ship, HUD."
  [world snap drag]
  (rl/clear-background (rl/rgba 8 12 24 255))
  (draw-vector-field! world)
  (draw-attractors! world)
  (draw-deathzones! world)
  (draw-prizes! world)
  (draw-ship! world)
  (draw-drag! snap drag)
  (draw-hud! world))
