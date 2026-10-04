(ns graviton.input
  "Raylib input reads ONLY, as intents: mouse position, drag edges, the
  restart key. Owns no game state."
  (:require [graviton.raylib :as rl]))

(defn snapshot
  "One frame of input as pure data. :down-x/:down-y is where the current
  drag began (nil when no drag is held)."
  []
  {:mx (rl/get-mouse-x)
   :my (rl/get-mouse-y)
   :pressed? (rl/mouse-pressed? rl/MOUSE-LEFT)
   :down? (rl/mouse-down? rl/MOUSE-LEFT)
   :released? (rl/mouse-released? rl/MOUSE-LEFT)
   :launch? (rl/key-pressed? rl/KEY-SPACE)
   :restart? (rl/key-pressed? rl/KEY-R)})

(defn drag
  "Fold a snapshot into drag state {:down-x :down-y}. Pressing starts a
  drag at the mouse; releasing ends it (state nil)."
  [snap drag]
  (cond
    (:pressed? snap) {:down-x (:mx snap) :down-y (:my snap)}
    (:released? snap) nil
    :else drag))

(defn drag-length
  "Euclidean length of the in-progress drag, 0 when none."
  [snap drag]
  (if drag
    (Math/sqrt (+ (Math/pow (- (:mx snap) (:down-x drag)) 2)
                  (Math/pow (- (:my snap) (:down-y drag)) 2)))
    0.0))
