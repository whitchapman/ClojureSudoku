(ns sudoku.algorithms.backtracking
  (:require [sudoku.data :as data]))

;;------------------------------------------------------------------------

(defn- run-backtracking* [grid unsolveds]
  (if-let [[[pos related-positions] & unsolveds] (seq unsolveds)]
    (loop [vals (get grid pos)]
      (when-let [[val & vals] (seq vals)]
        (let [grid (reduce (fn [grid pos]
                             (assoc grid pos (disj (get grid pos) val)))
                           (data/assign-grid-value grid pos val)
                           related-positions)]
          (or (run-backtracking* grid unsolveds)
              (recur vals)))))
    grid))

(defn run-backtracking [data]
  (let [grid (:grid data)
        unsolveds (keep-indexed (fn [pos cell]
                                  (when-not (data/cell-solved? cell)
                                    [pos (data/get-related-positions pos)]))
                                grid)]
    (when-let [grid (run-backtracking* grid unsolveds)]
      (assoc data :grid grid))))

;;------------------------------------------------------------------------
