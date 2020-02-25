(ns adv.graph
  (:import (javax.swing JFrame WindowConstants)
           (java.awt Color)))

(defn to-java-color [color]
  ({:black (. Color BLACK)
    :light-grey (. Color LIGHT_GRAY)
    :green (. Color GREEN)
    :red (. Color RED)
    :yellow (. Color YELLOW)
    :blue (. Color BLUE)} color))

(defn- paint [dots {:keys [xmin xmax ymin ymax]} g]
  (let [r (.getClipBounds g)
        xa (.x r)
        ya (.y r)
        xb (+ xa (.width r))
        yb (+ ya (.height r))
        qx (/ (- xb xa) (- xmax xmin))
        qy (/ (- yb ya) (- ymax ymin))]
    (doto g
      (.setColor (. Color WHITE))
      (.fillRect xa ya (.width r) (.height r)))
    (doseq [[[x y] color] dots]
      (.setColor g (to-java-color color))
      (.fillRect g (+ xa (* qx (- x xmin))) 
                   (+ ya (* qy (- y ymin)))
                   qx
                   qy))))

(defn show [dots bounds]
  (doto 
    (proxy [JFrame] [] (paint [g] (paint dots bounds g)))
    (.setSize 600 600)
    (.setLocationRelativeTo nil)
    (.setDefaultCloseOperation (WindowConstants/EXIT_ON_CLOSE))
    (.setVisible true)) nil)
  
