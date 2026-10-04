(ns graviton.test-runner
  "jolt -M:test runs every *-test and *-spec namespace under test/."
  (:require [clojure.test :as t])
  (:gen-class))

(defn -main
  [& _]
  (require '[graviton.physics-test])
  (require '[graviton.spawn-test])
  (require '[graviton.world-test])
  (require '[graviton.render-test])
  (require '[graviton.physics-spec])
  (require '[graviton.world-spec])
  (let [{:keys [fail error]} (t/run-tests 'graviton.physics-test 'graviton.spawn-test 'graviton.world-test 'graviton.render-test 'graviton.physics-spec 'graviton.world-spec)]
    (System/exit (if (zero? (+ fail error)) 0 1))))
