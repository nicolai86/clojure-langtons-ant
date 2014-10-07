(ns langtons-ant
  (:import (java.awt Color Dimension)
           (javax.swing JPanel JFrame Timer JOptionPane)
           (java.awt.event ActionListener KeyListener)))

(defn black-square? [square]
  (= true square))

(defn create-ant [x y]
  {:direction :up
   :x x
   :y y
   :last-x x
   :last-y y})

(defn turn-left [direction]
  (cond
   (= :up direction) :left
   (= :left  direction) :down
   (= :down  direction) :right
   (= :right  direction) :up))

(defn turn-right [direction]
  (cond
   (= :up direction) :right
   (= :right direction) :down
   (= :down direction) :left
   (= :left direction) :up))

(defn move-ant [{:keys [direction] :as ant}]
  (let [ant (assoc (assoc ant :last-x (:x ant)) :last-y (:y ant))]
  (cond
   (= :up direction) (assoc ant :y (- (:y ant) 1))
   (= :right direction) (assoc ant :x (+ (:x ant) 1))
   (= :down direction) (assoc ant :y (+ (:y ant) 1))
   (= :left direction) (assoc ant :x (- (:x ant) 1)))))

(defn step [board ant]
  (dosync
   (let [row (nth @board (:x @ant))
         square (nth row (:y @ant))]
     ; flip color
     (aset-boolean row (:y @ant) (not square))

     ; At a white square, turn 90° right, flip the color of the square, move forward one unit
     ; At a black square, turn 90° left, flip the color of the square, move forward one unit
     (if (black-square? square)
       (ref-set ant (assoc @ant :direction (turn-left (:direction @ant))))
       (ref-set ant (assoc @ant :direction (turn-right (:direction @ant)))))

     (ref-set ant (move-ant @ant)))))

(def point-size 10)
(def width 75)
(def height 50)

(defn point-to-screen-rect [pt]
  (map #(* point-size %)
       [(pt 0) (pt 1) 1 1]))

(defn fill-point [g pt color]
  (let [[x y width height] (point-to-screen-rect pt)]
    (.setColor g color)
    (.fillRect g x y width height)))

(defn paint-board [g board]
  (dorun (for [x (range (alength board))]
    (let [row (nth board x)]
      (dorun (for [y (range (alength row))]
              (if (black-square? (nth row y))
                (fill-point g [x y] (Color. 0 0 0))
                (fill-point g [x y] (Color. 255 255 255)))
              ))))))

(defn paint-ant [g {:keys [x y last-x last-y]}]
  (fill-point g [x y] (Color. 15 160 70)))

(defn game-panel [frame board ant]
  (proxy [JPanel ActionListener KeyListener] []
    (paintComponent [g]
                    (proxy-super paintComponent g)
                    (paint-board g @board)
                    (paint-ant g @ant))
    (keyPressed [e])
    (keyReleased [e])
    (keyTyped [e])
    (actionPerformed [e]
      (step board ant)
      (.validate frame)
      (.repaint frame))
    (getPreferredSize []
                      (Dimension. (* (inc width) point-size)
                                  (* (inc height) point-size)))))

(defn simulation []
  (let [board (ref (make-array Boolean/TYPE 101 101))
        ant (ref (create-ant 50 50))
        frame (JFrame. "langtons ant")
        panel (game-panel frame board ant)
        timer (Timer. 75 panel)]
    (doto panel
      (.setFocusable true)
      (.addKeyListener panel))
    (doto frame
      (.add panel)
      (.pack)
      (.setVisible true))
    (.start timer)
    [board, ant, timer]))