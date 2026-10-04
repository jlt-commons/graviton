(ns graviton.raylib
  "raylib over its C ABI via jolt.ffi, trimmed to the 2D calls graviton
  needs. Homebrew's lib dir is not on Chez's default dlopen search path,
  so the absolute path is listed first in deps.edn."
  (:require [jolt.ffi :as ffi]))

;; --- Color -------------------------------------------------------------------
(defn rgba
  "Pack an RGBA color into the little-endian uint32 that raylib's `Color`
  struct is (r | g<<8 | b<<16 | a<<24), so it can cross the FFI boundary
  as a :uint."
  [r g b a]
  (bit-or (int r) (bit-shift-left (int g) 8)
          (bit-shift-left (int b) 16) (bit-shift-left (int a) 24)))

(def LIGHTGRAY (rgba 200 200 200 255))   (def GRAY       (rgba 130 130 130 255))
(def DARKGRAY  (rgba 80 80 80 255))      (def YELLOW     (rgba 253 249 0 255))
(def GOLD      (rgba 255 203 0 255))     (def ORANGE     (rgba 255 161 0 255))
(def PINK      (rgba 255 109 194 255))   (def RED        (rgba 230 41 55 255))
(def MAROON    (rgba 190 33 55 255))     (def GREEN      (rgba 0 228 48 255))
(def LIME      (rgba 0 158 47 255))      (def DARKGREEN  (rgba 0 117 44 255))
(def SKYBLUE   (rgba 102 191 255 255))   (def BLUE       (rgba 0 121 241 255))
(def DARKBLUE  (rgba 0 82 172 255))      (def PURPLE     (rgba 200 122 255 255))
(def VIOLET    (rgba 135 60 190 255))    (def DARKPURPLE (rgba 112 31 126 255))
(def BEIGE     (rgba 211 176 131 255))   (def BROWN      (rgba 127 106 79 255))
(def DARKBROWN (rgba 76 63 47 255))      (def WHITE      (rgba 255 255 255 255))
(def BLACK     (rgba 0 0 0 255))         (def MAGENTA    (rgba 255 0 255 255))
(def RAYWHITE  (rgba 245 245 245 255))
(def BLANK     (rgba 0 0 0 0))

;; --- window / lifecycle ------------------------------------------------------
(ffi/defcfn init-window    "InitWindow"   [:int :int :string] :void)
(ffi/defcfn set-target-fps "SetTargetFPS" [:int] :void)
(ffi/defcfn close-window   "CloseWindow"  [] :void)
(ffi/defcfn ^:private should-close-raw "WindowShouldClose" [] :int)

;; --- frame -------------------------------------------------------------------
(ffi/defcfn begin-drawing    "BeginDrawing"    [] :void)
(ffi/defcfn end-drawing      "EndDrawing"      [] :void)
(ffi/defcfn clear-background "ClearBackground" [:uint] :void)
(ffi/defcfn get-frame-time   "GetFrameTime"    [] :float)

;; --- 2D drawing (scalar variants; Color is the only by-value struct) ---------
(ffi/defcfn draw-text            "DrawText"            [:string :int :int :int :uint] :void)
(ffi/defcfn measure-text         "MeasureText"         [:string :int] :int)
(ffi/defcfn draw-fps             "DrawFPS"             [:int :int] :void)
(ffi/defcfn draw-line            "DrawLine"            [:int :int :int :int :uint] :void)
(ffi/defcfn draw-rectangle       "DrawRectangle"       [:int :int :int :int :uint] :void)
(ffi/defcfn draw-rectangle-lines "DrawRectangleLines"  [:int :int :int :int :uint] :void)
(ffi/defcfn draw-circle          "DrawCircle"          [:int :int :float :uint] :void)
(ffi/defcfn draw-circle-lines    "DrawCircleLines"     [:int :int :float :uint] :void)
(ffi/defcfn fade-raw             "Fade"                [:uint :float] :uint)

(defn fade
  "raylib Fade: the same color with the given alpha [0 1]."
  [color a]
  (fade-raw color (double a)))

;; --- input -------------------------------------------------------------------
(ffi/defcfn ^:private key-down-raw      "IsKeyDown"           [:int] :int)
(ffi/defcfn ^:private key-pressed-raw   "IsKeyPressed"        [:int] :int)
(ffi/defcfn ^:private mouse-down-raw    "IsMouseButtonDown"   [:int] :int)
(ffi/defcfn ^:private mouse-pressed-raw "IsMouseButtonPressed" [:int] :int)
(ffi/defcfn ^:private mouse-released-raw "IsMouseButtonReleased" [:int] :int)
(ffi/defcfn get-mouse-x      "GetMouseX"         [] :int)
(ffi/defcfn get-mouse-y      "GetMouseY"         [] :int)
(ffi/defcfn get-random-value "GetRandomValue"    [:int :int] :int)

;; --- rlgl immediate mode (scalar; DrawTriangle needs Vector2 by value) -------
(ffi/defcfn rl-begin     "rlBegin"     [:int] :void)
(ffi/defcfn rl-end       "rlEnd"       [] :void)
(ffi/defcfn ^:private rl-vertex-2f-raw "rlVertex2f" [:float :float] :void)
(ffi/defcfn rl-color-4ub "rlColor4ub"  [:int :int :int :int] :void)
(def ^:const RL-TRIANGLES 4)

(defn- rl-vertex-2f
  [x y]
  (rl-vertex-2f-raw (double x) (double y)))

(defn- rl-color!
  "rlColor4ub from a packed rgba Color."
  [color]
  (rl-color-4ub (bit-and color 0xff)
                (bit-and (bit-shift-right color 8) 0xff)
                (bit-and (bit-shift-right color 16) 0xff)
                (bit-shift-right color 24)))

(defn triangle!
  "A filled triangle via rlgl immediate mode (DrawTriangle takes Vector2
  args by value, which jolt.ffi cannot express). :p1 :p2 :p3 are [x y]."
  [& {:keys [p1 p2 p3 color]}]
  (rl-begin RL-TRIANGLES)
  (rl-color! color)
  (rl-vertex-2f (nth p1 0) (nth p1 1))
  (rl-vertex-2f (nth p2 0) (nth p2 1))
  (rl-vertex-2f (nth p3 0) (nth p3 1))
  (rl-end))

;; --- screenshot hook plumbing (headless smoke tests) -------------------------
(ffi/defcfn take-screenshot       "TakeScreenshot"          [:string] :void)
(ffi/defcfn ^:private flush-batch "rlDrawRenderBatchActive" [] :void)

;; C-bool returns arrive in the low byte; mask so only 0/1 counts.
(defn window-should-close?
  []
  (not (zero? (bit-and (should-close-raw) 0xff))))

(defn key-down?
  [k]
  (not (zero? (bit-and (key-down-raw k) 0xff))))

(defn key-pressed?
  [k]
  (not (zero? (bit-and (key-pressed-raw k) 0xff))))

(defn mouse-down?
  [b]
  (not (zero? (bit-and (mouse-down-raw b) 0xff))))

(defn mouse-pressed?
  [b]
  (not (zero? (bit-and (mouse-pressed-raw b) 0xff))))

(defn mouse-released?
  [b]
  (not (zero? (bit-and (mouse-released-raw b) 0xff))))

;; --- constants (raylib KeyboardKey / MouseButton) ----------------------------
(def ^:const KEY-SPACE 32)  (def ^:const KEY-R     82)
(def ^:const KEY-ESCAPE 256) (def ^:const KEY-ENTER 257)
(def ^:const KEY-RIGHT 262) (def ^:const KEY-LEFT 263)
(def ^:const KEY-DOWN 264)  (def ^:const KEY-UP 265)
(def ^:const MOUSE-LEFT 0)
(def ^:const MOUSE-RIGHT 1)

;; --- ergonomic keyword-argument drawing API ----------------------------------
(defn window!
  "InitWindow with keyword args. :width :height :title."
  [& {:keys [width height title]
      :or {width 800
           height 450
           title "raylib"}}]
  (init-window width height title))

(defn text!
  "DrawText. :x :y :size :color. Coords cast to int — callers compute
  centered positions and may hand us ratios (e.g. (/ (- 800 w) 2))."
  [s & {:keys [x y size color]
        :or {x 0
             y 0
             size 20
             color BLACK}}]
  (draw-text s (int x) (int y) size color))

(defn text-width
  "MeasureText. :size."
  [s & {:keys [size]
        :or {size 20}}]
  (measure-text s size))

(defn fps!
  "DrawFPS. :x :y."
  [& {:keys [x y]
      :or {x 10
           y 10}}]
  (draw-fps x y))

(defn rect!
  "DrawRectangle. :x :y :width :height :color."
  [& {:keys [x y width height color]
      :or {x 0
           y 0
           width 10
           height 10
           color BLACK}}]
  (draw-rectangle x y width height color))

(defn rect-lines!
  "DrawRectangleLines. :x :y :width :height :color."
  [& {:keys [x y width height color]
      :or {x 0
           y 0
           width 10
           height 10
           color BLACK}}]
  (draw-rectangle-lines x y width height color))

(defn circle!
  "DrawCircle. :x :y :radius :color. Centers cast to int (raylib takes
  int centers, float radius)."
  [& {:keys [x y radius color]
      :or {x 0
           y 0
           radius 10
           color BLACK}}]
  (draw-circle (int x) (int y) (double radius) color))

(defn circle-lines!
  "DrawCircleLines. :x :y :radius :color. Centers cast to int."
  [& {:keys [x y radius color]
      :or {x 0
           y 0
           radius 10
           color BLACK}}]
  (draw-circle-lines (int x) (int y) (double radius) color))

(defn line!
  "DrawLine. :x1 :y1 :x2 :y2 :color. Coords cast to int."
  [& {:keys [x1 y1 x2 y2 color]
      :or {x1 0
           y1 0
           x2 0
           y2 0
           color BLACK}}]
  (draw-line (int x1) (int y1) (int x2) (int y2) color))

;; --- smoke-test loop guards --------------------------------------------------
(defn auto-quit-deadline
  "RAYLIB_APP_AUTO_QUIT_MS=<n> ends the loop after n ms, so the game is
  smoke-testable with no person at the keyboard. Returns an absolute ms
  deadline or nil."
  []
  (when-let [v (System/getenv "RAYLIB_APP_AUTO_QUIT_MS")]
    (try (let [ms (Integer/parseInt v)]
           (when (pos? ms) (+ (System/currentTimeMillis) ms)))
         (catch Exception _ nil))))

(defn keep-running?
  "True while the window is open and any RAYLIB_APP_AUTO_QUIT_MS deadline
  is unmet."
  [deadline]
  (and (not (window-should-close?))
       (or (nil? deadline) (< (System/currentTimeMillis) deadline))))

(def ^:private shot-path (System/getenv "RAYLIB_APP_SHOT"))

(defn maybe-screenshot!
  "RAYLIB_APP_SHOT=/path dumps one PNG on frame `at`, headless visual proof
  a frame rendered. Flushes raylib's batched geometry first (DrawText etc.
  is deferred until EndDrawing, so a mid-frame TakeScreenshot would miss
  it)."
  [frame at]
  (when (and shot-path (= frame at))
    (flush-batch)
    (take-screenshot shot-path)))

(defn screenshot!
  "On-demand PNG dump for the live REPL loop: flush the batch, shoot."
  [path]
  (flush-batch)
  (take-screenshot path))
