(ns reactor.ex-services
  (:require [reactor.core :as r]))

;; Demonstrates
;; - how to use asynchronous execution of potentially long-running fns (using in-future)
;; - how to create a timeout (using just, delay, merge, take)
;; - how to request a service periodically (using timer and flatmap)

;; (start) starts the program

(defn stockprice
  []
  (Thread/sleep (rand-nth [200 2000]))
  (rand-int 1000))


(defn stockprice-or-default
  [_]
  (->> (r/merge (r/just (r/in-future stockprice))
                (->> -1 r/just (r/delay 1000)))
       (r/take 1)))

(defn stockprices-stream
  []
  (->> (r/timer 1000)
       (r/flatmap stockprice-or-default)
       (r/subscribe println)))

