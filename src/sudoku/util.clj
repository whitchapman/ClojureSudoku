(ns sudoku.util
  (:require [clojure.set :as set]))


;;------------------------------------------------------------------------
;;utility

;;generate all possible combinations of (range 0 n) of size x
(defn generate-combinations [n x]
  (defn f [prefix es x]
    (loop [es es acc []]
      (if-let [[e & es] (seq es)]
        (let [prefix (conj prefix e)
              acc (if (= x 1)
                    (conj acc prefix)
                    (concat acc (f prefix es (dec x))))]
          (recur es acc))
        acc)))
  (f [] (range 0 n) x))
