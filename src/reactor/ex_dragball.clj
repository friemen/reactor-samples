(ns reactor.ex-dragball
  (:require [reactor.swing-support :as s]
            [reactor.core :as r])
  (:import [javax.swing JPanel]))

;; Demonstrates how dragging of a shape can be implemented
;; - The mouse events are filtered
;; - The selection is determined by mapping a function
;;   over all shapes and the event
;; - Mouse motion is followed by another stream with an fn mapping
;;   that uses the selection to update the shapes
;; - Repainting is triggered by updated shapes and throttled to 20/sec

;; (start) starts the program


(def ball-image (s/load-image "ball.png"))


(defn selected-shapes
  [all-shapes {:keys [trigger x y] :as evt}]
  (case trigger
    :pressed (s/find-shape-ids all-shapes evt)
    :released nil))

(defn new-ball
  [all-shapes _]
  (let [size (rand-nth [20 30 40])
        ball (s/make-image ball-image 200 200 size size)]
    (assoc all-shapes (:id ball) ball)))

(defn start
  []
  (r/with
   (r/network)
   (let [all-shapes (r/behavior (s/make-shapes-map
                                 [(s/make-image ball-image 100 100 40 40)
                                  (s/make-image ball-image 50  100 30 30)
                                  (s/make-image ball-image 200  10 30 30)])
                                :label "shapes")
         p          (s/shapes-panel all-shapes)
         f          (s/frame "Move pieces" p 400 400)
         mouse      (s/mouse-events p)
         keys       (s/keyboard-events f)
         selected   (->> mouse
                         (r/filter #(contains? #{:pressed :released} (:trigger %)))
                         (r/map selected-shapes all-shapes)
                         (r/map first)
                         (r/hold))]
     (->> mouse
          (r/map s/move-shape all-shapes selected)
          (r/into all-shapes))
     (->> keys
          (r/filter (partial s/key? :typed \+))
          (r/map new-ball all-shapes)
          (r/into all-shapes))
     (->> all-shapes
          (r/throttle (fn [_] (.repaint p)) 50 1)))))
