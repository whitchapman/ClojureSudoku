(ns sudoku.util
  (:require [clojure.set :as set]))

;;------------------------------------------------------------------------

(defn- generate-combinations* [size xs] ;;tail recursive
  (if (= size 0)
    (map first xs) ;;return the prefixes of length original size
    (->> xs
         (mapcat (fn [[prefix elements]]
                   (if-not elements
                     [[prefix]]
                     (let [num-elements (count elements)]
                       (if (= num-elements size)
                         [[(apply conj prefix elements)]] ;;leave no more elements to process
                         (loop [i (- num-elements size) elements elements acc []]
                           (if (>= i 0)
                             (let [[element & elements] elements]
                               (recur (dec i) elements (conj acc [(conj prefix element) elements])))
                             acc)))))))
         (generate-combinations* (dec size)))))

(defn generate-seq-combinations [elements x]
  (let [num-elements (count elements)]
    (when (and (> x 0) (>= num-elements x))
      (if (= num-elements x)
        [(vec elements)] ;;optimization for selecting all elements
        (vec (generate-combinations* x [[[] elements]]))))))

(defn generate-range-combinations [n x]
  (generate-seq-combinations (range n) x))

;;------------------------------------------------------------------------
