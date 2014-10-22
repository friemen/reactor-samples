(ns reactor.ex-switch
  (:require [reactor.core :as r]
            [reactor.swing-support :as s])
  (:import [javax.swing JButton JLabel]
           [java.awt Color]))


;; Demonstrates how switch can be used to follow
;; a certain eventsource or behavior

;; (start) starts the program

(defn new-number
  []
  (rand-nth [42 1 2 3] ))

(defn make-panel
  []
  (doto (s/panel "flowy" "" "")
    (.add (JButton. "Start"))
    (.add (JLabel. "A number"))
    (.add (JButton. "Stop"))))


(defn start
  []
  (let [p        (make-panel)
        f        (s/frame "Switching state" p 300 100)
        numbers  (r/sample 500 new-number)
        actions  (r/merge (-> f (s/get-component [0]) s/action-events)
                          (-> f (s/get-component [2]) s/action-events))
        label    (s/get-component f [1])]
    (->> actions
         (r/map (fn [{:keys [value]}]
                      (case value
                        "Start" (r/concat (r/take-t 4000 numbers)
                                          (r/just "A number"))
                        "Stop"  (r/behavior "A number"))))
         (r/switch)
         (r/subscribe #(.setText label (str %))))
    f))
