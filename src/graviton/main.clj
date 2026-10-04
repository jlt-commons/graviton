(ns graviton.main
  "Window setup + the frame loop: input -> world step -> render. Drag on
  the screen to place gravity wells during the build phase; SPACE releases
  the ship to ride the field to the prizes; too many wells grow death
  zones. R restarts."
  (:require [graviton.render :as render]
            [graviton.raylib :as rl]
            [graviton.input :as gin]
            [graviton.world :as w]))

(def WIDTH 800)
(def HEIGHT 600)

;; the field math is per-tick (60 ticks a second, like the original's
;; ticker); the clock hands over seconds
(def TICKS-PER-SECOND 60.0)

;; headless smoke: scripted drags [[start-frame end-frame x y] ...] so a
;; screenshot captures a live field with wells placed
(def ^:private scripted-drags
  (when-let [v (System/getenv "GRAVITON_APP_DRAGS")]
    (try (read-string v) (catch Exception _ nil))))

;; headless smoke: which frame SPACE is pressed, to script a launch
(def ^:private scripted-launch-frame
  (when-let [v (System/getenv "GRAVITON_APP_LAUNCH_FRAME")]
    (try (Integer/parseInt v) (catch Exception _ nil))))

(def ^:private shot-frame
  (or (when-let [v (System/getenv "GRAVITON_APP_SHOT_FRAME")]
        (try (Integer/parseInt v) (catch Exception _ nil)))
      90))

(defn rng
  "The shell's rng: raylib's own generator, matching the injected contract."
  []
  (rl/get-random-value 0 2147483647))

(defn -main
  [& _]
  (rl/window! :width WIDTH :height HEIGHT :title "graviton")
  (rl/set-target-fps 60)
  (let [deadline (rl/auto-quit-deadline)]
    (loop [frame 0
           world (w/initial WIDTH HEIGHT rng)
           drag nil]
      (if (rl/keep-running? deadline)
        (let [snap (gin/snapshot)
              ;; a scripted drag begins on its start frame
              scripted-start (some (fn [d] (when (= frame (nth d 0)) d)) scripted-drags)
              scripted-end (some (fn [d] (when (= frame (nth d 1)) d)) scripted-drags)
              drag (if scripted-start
                     {:down-x (nth scripted-start 2) :down-y (nth scripted-start 3)}
                     drag)
              ;; a drag ends on release (real) or its scripted end frame
              released? (or (:released? snap) (some? scripted-end))
              ;; placing the well uses the drag's origin and length
              world (if (and released? drag)
                      (w/add-attractor world (:down-x drag) (:down-y drag)
                                       (gin/drag-length snap drag))
                      world)
              drag (if released? nil (gin/drag snap drag))
              launch? (or (:launch? snap) (= frame scripted-launch-frame))
              world (if launch? (w/launch world) world)
              restart? (or (:restart? snap)
                           ;; smoke mode: restart keeps the run alive longer
                           (and scripted-drags (= frame (+ 30 shot-frame))))
              world (if restart? (w/restart world w/TOTAL-PRIZES rng) world)
              world' (if (contains? #{:playing :won :lost} (:phase world))
                       (w/step world (* TICKS-PER-SECOND
                                        (min 0.05 (double (rl/get-frame-time)))))
                       world)]
          (rl/begin-drawing)
          (render/draw-world! world' snap drag)
          (rl/maybe-screenshot! frame shot-frame)
          (rl/end-drawing)
          (recur (inc frame) world' drag))
        (rl/close-window)))))
