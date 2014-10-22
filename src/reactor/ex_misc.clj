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

(defn make-calculator
  []
  (r/setup-network "z=x*(x+y)"
                   x (r/behavior 0)
                   y (r/behavior 0)
                   z (r/lift (* x (+ x y 2)))))


(defn make-decision
  []
  (r/setup-network ""
                   weather (r/behavior nil)
                   temp    (r/behavior 21)
                   vehicle (r/lift (case weather
                                     "sunshine" "bicycle"
                                     "rain"     "bicycle"
                                     "snow"     "car"
                                     "undefined"))))

#_ (def c (make-decision))
#_ (r/push! c :weather "snow")

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



(defn stock-price
  []
  (rand-nth (range 1 10)))


(defn sliding-buffer
  [n]
  (fn [buf x]
    (conj (vec (drop (- (count buf) (dec n)) buf)) x)))


(defn increasing?
  [xs]
  (->> xs
       (map vector (drop 1 xs))
       (every? (partial apply >))))

(defn send-mail!
  [prices]
  (println "Increasing prices!" prices))


#_(def n (r/network "price-checker"))

#_(r/with n (->> (r/sample 1000 stock-price)
                 (r/subscribe println)
                 (r/scan (sliding-buffer 3) [])
                 (r/subscribe println)
                 (r/filter #(>= (count %) 3))
                 (r/filter increasing?)
                 (r/subscribe send-mail!)))


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
