(ns adventofcode18.advent14
  (:require [clojure.string :as str]))

(def input [3 7])

(defn next-recipes [a b]
  (map (fn [x] (Integer/parseInt (str x)))
       (vec (str (+ a b)))))

(defn print-recipes [recipes a b]
  (dorun
    (map-indexed
      (fn [idx x] (if (= idx a)
                    (printf "(%d)" x)
                    (if (= idx b)
                      (printf "[%d]" x)
                      (printf " %d " x))))
      (seq recipes)))
  (println))

(defn next-10 [recipes n] (take 10 (drop n recipes)))

(defn do-step [recipes recipes-count elf-1-pos elf-2-pos recipe-limit]
  (let [elf-1-score (nth recipes elf-1-pos)
        elf-2-score (nth recipes elf-2-pos)
        new-recipes (next-recipes elf-1-score elf-2-score)
        all-recipes (into recipes new-recipes)
        all-recipe-count (+ recipes-count (count new-recipes))
        new-elf1 (mod (+ elf-1-pos (+ elf-1-score 1)) all-recipe-count)
        new-elf2 (mod (+ elf-2-pos (+ elf-2-score 1)) all-recipe-count)]
    #_(print-recipes all-recipes new-elf1 new-elf2)
    (if (< all-recipe-count (+ recipe-limit 10))
      (recur all-recipes all-recipe-count new-elf1 new-elf2 recipe-limit)
      (next-10 all-recipes recipe-limit))))

(defn run1 [recipe-limit]
  (str/join (do-step input (count input) 0 1 recipe-limit)))

