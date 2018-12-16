(ns adventofcode18.advent02
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(defn load-input-file [filename]
  (->> (slurp filename)
       (str/split-lines)))

(defn get-n-count [n freqs]
  (count (filter (fn [[_ v]] (= v n)) freqs)))

(defn count-freq [n freqs-seq]
  (count
    (filter (fn [x] (not= x 0))
            (map (partial get-n-count n) freqs-seq))))

(defn check-sum [filename]
  (let [freqs-seq (->> (load-input-file filename)
                       (map frequencies))
        twos (count-freq 2 freqs-seq)
        threes (count-freq 3 freqs-seq)]
    (* twos threes)))


(def test-input (load-input-file "resources/testinput2.txt"))

(defn compare-two [a b]
  (let [pairs (map vector a b)
        one-less (- (count pairs) 1)
        same (filter (fn [[x y]] (= x y)) pairs)]
    (if (= (count same) one-less)
      (str/join (map first same)))))

(defn possible-pairs [inputs] (combo/combinations inputs 2))

(defn proto-box [filename]
  (->>
    (load-input-file filename)
    (possible-pairs)
    (map (fn [[x y]] (compare-two x y)))
    (filter some?)))