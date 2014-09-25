(ns reactor.swing-support
  (:require [reactnet.core :as rn]
            [reactor.core :as r])
  (:import [javax.swing JFrame JPanel]
           [javax.swing.event DocumentListener ListSelectionListener]
           [java.awt Color]
           [java.awt.event ActionListener KeyListener MouseListener MouseMotionListener]
           [javax.imageio ImageIO]
           [net.miginfocom.swing MigLayout]))


;; ------------------------------------------------------------------------
;; Support for drawing shapes

(def next-id (atom 1))

(defprotocol IShape
  (draw [shape graphics])
  (in-shape? [shape x y]))

(defrecord Oval [id x y xd yd]
  IShape
  (draw [this g]
    (.drawOval g x y xd yd))
  (in-shape? [this x' y']
    (and (>= x' x)
         (>= y' y)
         (< x' (+ x xd))
         (< y' (+ y yd)))))


(defrecord Image [id img x y w h]
  IShape
  (draw [this g]
    (.drawImage g img x y w h nil))
  (in-shape? [this x' y']
    (and (>= x' x)
         (>= y' y)
         (< x' (+ x w))
         (< y' (+ y h)))))


(defn make-image
  [img x y w h]
  (Image. (swap! next-id inc) img x y w h))


(defn load-image
  [resourcename]
  (ImageIO/read (ClassLoader/getSystemResource resourcename)))


(defn find-shape-ids
  [shapes {:keys [x y]}]
  (->> shapes
       vals
       (filter #(in-shape? % x y))
       (mapv (juxt :id #(- x (:x %)) #(- y (:y %))))))


(defn move-shape
  [shapes [id dx dy] {:keys [x y]}]
  (if (integer? id)
    (update-in shapes [id] assoc :x (- x dx) :y (- y dy))
    shapes))


(defn make-shapes-map
  [shapes-seq]
  (->> shapes-seq
       (map (juxt :id identity))
       (into {})))


(defn shapes-panel [shapes]
  (doto (proxy [JPanel] []
          (paint [g]
            (proxy-super paintComponent g)
            (doseq [s (vals @shapes)] (draw s g))))
    (.setBackground (Color. 10 180 30))))


;; ------------------------------------------------------------------------
;; Handling Swing UI


(defn layout
  [general-settings column-settings row-settings]
  (MigLayout. general-settings column-settings row-settings))


(defn panel
  [lygeneral lycols lyrows]
  (doto (JPanel.)
    (.setLayout (layout lygeneral lycols lyrows))))


(defn frame
  [title content-pane w h]
  (doto (JFrame. title)
    (.setVisible true)
    (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
    (.setSize w h)
    (.setContentPane content-pane)))


(defn close
  [frame]
  (.setVisible frame false))

(defn get-component
  [frame path]
  (reduce #(.getComponent %1 %2) (.getContentPane frame) path))



;; ------------------------------------------------------------------------
;; Factories to create eventstreams and behaviors from GUI components


(defn mouse-events
  [component]
  (let [netref rn/*netref*
        e      (r/eventstream :label "mouse")
        push!  #(r/push! netref e {:trigger %1
                                   :x (.getX %2)
                                   :y (.getY %2)
                                   :left (= 1 (.getButton %2))
                                   :right (= 3 (.getButton %2))})]
    (.addMouseListener component
                       (reify MouseListener
                         (mouseClicked [_ me] (push! :clicked me))
                         (mousePressed [_ me] (push! :pressed me))
                         (mouseReleased [_ me] (push! :released me))
                         (mouseEntered [_ me] (push! :entered me))
                         (mouseExited [_ me] (push! :exited me))))
    (.addMouseMotionListener component
                             (reify MouseMotionListener
                               (mouseDragged [_ me] (push! :dragged me))
                               (mouseMoved [_ me] (push! :moved me))))
    e))


(defn key?
  [expected-trigger expected-ch {:keys [trigger char]}]
  (and (= trigger expected-trigger) (= char expected-ch)))


(defn keyboard-events
  [component]
  (let [netref rn/*netref*
        e      (r/eventstream :label "keyboard")
        push!  #(r/push! netref e {:trigger %1
                                   :code (.getKeyCode %2)
                                   :char (.getKeyChar %2)
                                   :modifiers (.getModifiers %2)})]
    (.addKeyListener component
                     (reify KeyListener
                       (keyPressed [_ ke] (push! :pressed ke))
                       (keyReleased [_ ke] (push! :released ke))
                       (keyTyped [_ ke] (push! :typed ke))))
    e))


(defn action-events
  [component]
  (let [netref rn/*netref*
        e      (r/eventstream :label "actions")]
    (.addActionListener component (reify ActionListener
                                    (actionPerformed [_ ae]
                                      (r/push! netref e {:value (.getText component)}))))
    e))


(defn text-behavior
  [textcomponent]
  (let [netref   rn/*netref*
        get-text (fn [evt]
                   (let [d (.getDocument evt)]
                     (.getText d 0 (.getLength d))))
        b        (r/behavior (.getText textcomponent) :label "text")
        dl       (reify DocumentListener
                   (insertUpdate [_ evt]
                     (r/push! netref b (get-text evt)))
                   (removeUpdate [_ evt]
                     (r/push! netref b (get-text evt)))
                   (changedUpdate [_ evt]
                     (r/push! netref b (get-text evt))))
        doc      (.getDocument textcomponent)]
    (-> doc (.addDocumentListener dl))
    (->> b
         (r/subscribe (fn [t]
                        (.removeDocumentListener doc dl)
                        (.setText textcomponent t)
                        (.addDocumentListener doc dl))))
    b))


(defn listmodel-behavior
  [list]
  (let [model     (.getModel list)
        get-items #(-> model .elements enumeration-seq vec)
        new?      (atom false)]
    (reify
      reactnet.core.IReactive
      (next-value [_]
        [(get-items) (rn/now)])
      (available? [_] true)
      (pending? [_] @new?)
      (completed? [_] false)
      (consume! [m]
        (reset! new? false)
        [(get-items) (rn/now)])
      (deliver! [_ [items ts]]
        (.removeAllElements model)
        (doseq [i items]
          (.addElement model i))
        (reset! new? true))
      clojure.lang.IDeref
      (deref [_]
        (get-items)))))


(defn listselection-behavior
  [list]
  (let [netref   rn/*netref*
        b        (r/behavior (.getMinSelectionIndex list) :label "selection")
        sl       (reify ListSelectionListener
                   (valueChanged [_ evt]
                     (r/push! netref b (.getMinSelectionIndex list))))]
    (.addListSelectionListener list sl)
    b))
