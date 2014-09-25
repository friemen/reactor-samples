(ns reactor.ex-guessing
  (:require [reactor.core :as r]
            [reactor.swing-support :refer [frame action-events]])
  (:import [javax.swing JPanel JButton JLabel]
           [java.awt GridLayout]))

;; Demonstrates
;; - how to accumulate state (using scan)
;; - how to create a timeout (using delay, merge, take)

;; (start) starts the program

(defn make-game-panel
  []
  (let [a-button      (JButton. "A")
        b-button      (JButton. "B")
        input-label   (JLabel. "")
        message-label (JLabel. "")]
    {:panel (doto (JPanel.)
              (.add (JLabel. "Enter the magic-key within 5 secs:"))
              (.add a-button)
              (.add b-button)
              (.add input-label)
              (.add message-label))
     :a a-button
     :b b-button
     :input input-label
     :message message-label}))


(defn replace-contents
  [parent child]
  (doto parent
    (.removeAll)
    (.add child)
    (.validate)
    (.repaint)))


(def magic-key "ABBABA")
(def timeout 5000)

(defn play
  [game-panel]
  (r/with
   (r/network)
    (let [{:keys
           [panel
            a
            b
            input
            message]} (make-game-panel)
            text-e    (->> (r/merge (action-events a) (action-events b))
                           (r/map :value)
                           (r/scan str "")
                           (r/subscribe #(.setText input %)))
            checked-e (->> text-e
                           (r/map #(cond
                                    (= magic-key %)                 :right
                                    (not= (.indexOf magic-key %) 0) :wrong))
                           (r/remove nil?))
            timeout-e (->> (r/just :timeout)
                           (r/delay timeout))]
      (->> (r/merge checked-e timeout-e)
           (r/take 1)
           (r/map #(case %
                     :timeout "Too late!"
                     :right   "Congrats!"
                     :wrong   "Sorry."))
           (r/subscribe #(.setText message %)))
      (replace-contents game-panel panel))))


(defn start
  []
  (let [play-button (JButton. "Play!")
        game-panel  (JPanel.)
        main-panel  (doto (JPanel.)
                      (.add play-button)
                      (.add game-panel)
                      (.setLayout (GridLayout. 2 1 0 10)))]
    (->> (action-events play-button)
         (r/subscribe (fn [_] (play game-panel))))
    
    (frame magic-key main-panel 600 100)))
