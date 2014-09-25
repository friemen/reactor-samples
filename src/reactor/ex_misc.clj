(ns reactor.ex-misc
  (:require [reactor.core :as r]))

;; ---------------------------------------------------------------------------
;; you can do dataflow programming

(defmacro defb
  [symbol expr]
  `(def ~symbol (r/behavior ~expr)))

(defmacro defr
  [symbol expr]
  `(def ~symbol (r/lift ~expr)))


(def prefix "= ")

(defb x 1)
(defb y 12)

(defr z (str prefix (let [y2 (* 2 y)]
                      (if (> y2 10)
                        (+ x 10)
                        (+ x y2)))))

;; ---------------------------------------------------------------------------
;; simple processing chain

#_ (def e (r/seqstream (range 10)))

#_ (def c (->> e
            (r/map inc)
            (r/filter odd?)
            (r/subscribe println)))


;; ---------------------------------------------------------------------------
;; insert delay between two values
;; print :foo, 5 seconds later print :bar

#_ (->> (r/just :foo)
        (r/flatmap (fn [x]
                     (r/merge (r/just x)
                              (->> (r/just :bar) (r/delay 5000)))))
        (r/subscribe println))


;; ---------------------------------------------------------------------------
;; sampling

#_ (def rand-ints (r/sample 100 #(rand-int 100)))
#_ (->> rand-ints (r/subscribe println))

#_ (Thread/sleep 2000)
#_ (r/complete! rand-ints)




;; ---------------------------------------------------------------------------
;; Demonstrate switching

(defn make-switch-rs
  []
  (r/with (r/network)
    (let [i       (r/eventstream)
          source  (r/behavior nil)
          t       (r/timer 2000)
          result  (->> source r/switch (r/subscribe println))]
      
      {:network reactnet.core/*netref*
       :i i :t t :source source :result result
       :stop-fn #(r/complete! y)})))

(defn >!
  [system key value]
  (r/push! (:network system) (get system key) value))

(defn stop!
  [system]
  ((:stop-fn system)))
