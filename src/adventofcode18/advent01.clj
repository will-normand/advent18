(ns adventofcode18.advent01
  (:require [clojure.string :as str]))

;;; Day 1

(defn load-input-file [filename]
  (->> (slurp filename)
       (str/split-lines)
       (map (fn [x] (Integer/parseInt x)))))

(defn sum [seq] (reduce + seq))

(defn freq [filename]
  (sum (load-input-file filename)))

(defn inf-numbers [numbers] (lazy-seq (flatten (repeat numbers))))

(defn find-duplicate1 [nums current found]
  (let [next (+ current (first nums))]
    (if (contains? found next)
      next
      (recur (rest nums) next (conj found next)))))

(defn find-duplicate [nums] (find-duplicate1 nums 0 #{0}))

