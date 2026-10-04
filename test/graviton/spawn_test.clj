(ns graviton.spawn-test
  (:require [clojure.test :refer [deftest is testing]]
            [graviton.spawn :as spawn]))

;; deterministic LCG, the repo convention for reproducible "random" tests
(defn lcg [seed] (let [s (volatile! seed)] (fn [] (vswap! s #(rem (inc (* 1103515245 %)) 2147483648)))))

(defn- in-rect? [{:keys [x y radius]} w h]
  (and (>= (- x radius) 0) (<= (+ x radius) w)
       (>= (- y radius) 0) (<= (+ y radius) h)))

(deftest prizes-are-placed-inside-and-apart
  (let [rng (lcg 7)
        {:keys [width height] :as state} {:width 800 :height 600 :rng rng}
        world (spawn/prizes state 5)
        ps (:prizes world)]
    (is (= 5 (count ps)))
    (is (every? #(in-rect? % 800 600) ps) "every disk fully on screen")
    (is (= ps (distinct ps)))
    (testing "no two disks overlap"
      (is (every? (fn [[a b]]
                    (>= (Math/hypot (- (:x a) (:x b)) (- (:y a) (:y b)))
                        (+ (:radius a) (:radius b))))
                  (map vector ps (rest ps))))
      (is (every? (fn [i] (every? (fn [j] (or (= i j)
                                              (>= (Math/hypot (- (:x (nth ps i)) (:x (nth ps j)))
                                                              (- (:y (nth ps i)) (:y (nth ps j))))
                                                  (+ (:radius (nth ps i)) (:radius (nth ps j))))))
                                  (range (count ps))))
                  (range (count ps)))))
    (is (= 5 (:total-prizes world)))))

(deftest same-seed-same-placement
  (let [mk (fn [] (spawn/prizes {:width 800 :height 600 :rng (lcg 42)} 5))
        a (mk) b (mk)]
    (is (= (:prizes a) (:prizes b)))))

(deftest prizes-respect-existing-actors
  (let [big {:x 400 :y 300 :radius 400} ;; swallows the middle
        world (spawn/prizes {:width 800 :height 600 :rng (lcg 9) :existing [big]} 4)]
    (is (every? (fn [p] (>= (Math/hypot (- (:x p) 400) (- (:y p) 300))
                            (+ (:radius p) 400)))
                (:prizes world)))))

(deftest attractor-from-a-drag
  (let [rng (lcg 3)
        a (spawn/attractor {:rng rng} 100 200 60)]
    (is (= 100 (:x a)))
    (is (= 200 (:y a)))
    (is (= 60 (:radius a)) "radius from drag distance, capped at CAP")
    (is (= :attractor (:type a)))))

(deftest attractor-radius-is-capped
  (let [a (spawn/attractor {:rng (lcg 3)} 0 0 500)]
    (is (= spawn/RADIUS-CAP (:radius a)))
    (is (= (/ (* spawn/RADIUS-CAP spawn/RADIUS-CAP) 10) (:mass a)))))

(deftest maybe-deathzone-appears-when-attractors-pile-up
  (let [rng (lcg 11)
        ;; 7 attractors: (mod 7 4) > 2 -> zone spawns
        world (spawn/maybe-deathzone {:width 800 :height 600 :rng rng
                                      :attractors (repeat 7 {:x 0 :y 0 :radius 1})})]
    (is (= 1 (count (:deathzones world))))
    (is (every? #(in-rect? % 800 600) (:deathzones world))))
  (testing "four attractors stay clean"
    (is (empty? (:deathzones (spawn/maybe-deathzone {:width 800 :height 600
                                                     :rng (lcg 11)
                                                     :attractors (repeat 4 {})}))))))

(deftest same-seed-same-deathzone
  (let [mk (fn [] (spawn/maybe-deathzone {:width 800 :height 600 :rng (lcg 13)
                                          :attractors (repeat 7 {:x 0 :y 0 :radius 1})}))]
    (is (= (mk) (mk)))))
