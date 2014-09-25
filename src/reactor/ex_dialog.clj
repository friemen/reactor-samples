(ns reactor.ex-dialog
  (:require [reactor.core :as r]
            [reactor.swing-support :as s])
  (:import [javax.swing DefaultListModel JButton JLabel JList JPanel JTextField]))


;; Demonstrates a master-detail UI scenario
;; - functions opening a window return a stream to deliver the results
;; - these functions are invoked using flatmap

;; This example is very rudimentary,
;; for example there is no means to deactivate
;; the master window when the detail is shown

;; (start) starts the program


(defn remove-by-index
  [items i]
  (vec (concat
        (take i items)
        (drop (inc i) items))))


(defn detail-frame
  [items]
  (let [detail  (s/frame "Detail" (doto (s/panel "fillx, wrap 2" "[|grow]" "[|]")
                                    (.add (JLabel. "Name"))
                                    (.add (JTextField. "") "growx")
                                    (.add (doto (s/panel "ins 0" "[||grow]" "[]")
                                            (.add (JButton. "OK"))
                                            (.add (JButton. "Cancel")))
                                          "span"))
                         200 100)
        text-b  (->> (s/get-component detail [1])
                     (s/text-behavior))
        actions (->> (r/merge (->> (s/get-component detail [2 0]) (s/action-events))
                              (->> (s/get-component detail [2 1]) (s/action-events)))
                     (r/map :value))]
    (->> actions
         (r/subscribe (fn [_]
                        (s/close detail)
                        (r/complete! actions)))
         (r/map #(case %
                   "Cancel" items
                   "OK"     (conj items @text-b))))))


(defn master-action
  [[items sel action]]
  (case action
    "Add"     (detail-frame items)
    "Remove"  (r/just (remove-by-index items sel))
    (r/just items)))


(defn master-frame
  []
  (let [master  (s/frame "Master" (doto (s/panel "flowy, fill" "[]" "[|grow|]")
                                    (.add (JLabel. "Items"))
                                    (.add (JList. (DefaultListModel.)) "grow")
                                    (.add (doto (s/panel "ins 0" "[||grow]" "[]")
                                            (.add (JButton. "Add"))
                                            (.add (JButton. "Remove"))
                                            (.add (JButton. "Close")))))
                      600 400)
        listbox (s/get-component master [1])
        items-b (s/listmodel-behavior listbox)
        sel-b   (s/listselection-behavior listbox)
        actions (->> (r/merge (->> (s/get-component master [2 0]) (s/action-events))
                              (->> (s/get-component master [2 1]) (s/action-events))
                              (->> (s/get-component master [2 2]) (s/action-events)))
                     (r/map :value))]
    (->> actions
         (r/map vector items-b sel-b)
         (r/flatmap master-action)
         (r/into items-b))
    (->> actions
         (r/filter #{"Close"})
         (r/snapshot items-b)
         (r/subscribe (fn [_]
                        (s/close master)
                        (r/complete! actions))))))


(defn start
  []
  (->> (master-frame)
       (r/subscribe println)))
