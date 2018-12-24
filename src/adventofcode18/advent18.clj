(ns adventofcode18.advent18
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def areas {:ground \.
            :trees  \|
            :lumber \#})

(defn parse-area [area] ((set/map-invert areas) area))
(defn parse-row [row] (map parse-area (vec row)))
(defn load-input [filename]
  (map parse-row (str/split-lines (slurp filename))))

(defn print-area [area] (print (areas area)))
(defn print-row [row] (dorun (map print-area row)) (println))
(defn print-landscape [landscape]
  (dorun (map print-row landscape))
  (println)
  landscape)

(defn lookup [landscape x y]
  (nth (nth landscape y nil) x nil))

(defn adjacent-indices [x y]
  #_(filter (fn [[x y]] (and (> x 0) (> y 0))))
  [[(- x 1) (- y 1)]
   [(- x 1) y]
   [(- x 1) (+ y 1)]
   [x (- y 1)]
   [x (+ y 1)]
   [(+ x 1) (- y 1)]
   [(+ x 1) y]
   [(+ x 1) (+ y 1)]])

(defn count-neighbours [landscape x y]
  (let [neighbours (map
                     (fn [[x y]] (lookup landscape x y))
                     (adjacent-indices x y))]
    (frequencies neighbours)))

(defn new-state [landscape x y]
  (let [area (lookup landscape x y)
        neighbour-counts (count-neighbours landscape x y)]
    (if (= area :ground)
      (if (>= (get neighbour-counts :trees 0) 3)
        :trees
        :ground)
      (if (= area :trees)
        (if (>= (get neighbour-counts :lumber 0) 3)
          :lumber
          :trees)
        (if (= area :lumber)
          (if (and (>= (get neighbour-counts :trees 0) 1) (>= (get neighbour-counts :lumber 0) 1))
            :lumber
            :ground))))))

(defn tick [landscape]
   (map-indexed (fn [y row]
                 (map-indexed
                   (fn [x area] (new-state landscape x y))
                   row))
               landscape))

(defn count-resources [landscape]
  (reduce (fn [m n] (merge-with + m n)) {} (map (fn [row] (frequencies row)) landscape)))

(defn resource-value [landscape]
  (let [resources (count-resources landscape)]
    (println resources)
    (* (get resources :trees 0) (get resources :lumber 0))))

(defn run [landscape times]
  (if (= times 0)
    (do (print-landscape landscape) (resource-value landscape))
    (do (if (= (mod times 1000) 0) (println times))
        (recur (tick landscape) (- times 1)))))
