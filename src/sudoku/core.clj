(ns sudoku.core
  (:require [sudoku.algorithms.simplify-groups :as alg-simplify-groups]
            [sudoku.algorithms.locked-candidates :as alg-locked-candidates]
            [sudoku.algorithms.x-wing :as alg-x-wing]
            [sudoku.data :as data]
            [sudoku.puzzles :as puzzles]))

;;------------------------------------------------------------------------
;;assignment

(defn assign-values [data coordinate-val-triples]
  (let [reduce-step (fn [grid [x y val]]
                      (let [pos (+ (* (- x 1) 9) (- y 1))]
                        (data/assign-grid-value grid pos val)))]
    (->> (reduce reduce-step (:grid data) coordinate-val-triples)
         (assoc data :grid))))

(defn fake-solve-zero-fill [data]
  (->> (loop [grid (:grid data) pos 0]
         (if (< pos 81)
           (if (data/cell-solved? (get grid pos))
             (recur grid (inc pos))
             (recur (data/assign-grid-value grid pos 0) (inc pos)))
           grid))
       (assoc data :grid)))

;;------------------------------------------------------------------------
;;solution functions

(def algorithms
  [[:simplify-groups alg-simplify-groups/run-simplify-groups]
   [:locked-candidates alg-locked-candidates/run-locked-candidates]
   [:x-wing alg-x-wing/run-x-wing]])

(defn run-iteration [data & {:keys [algs] :or {algs algorithms}}]
  (loop [j 1 algs algs data data]
    (if-let [[[alg-key alg-fn] & algs] (seq algs)]
      (let [[data changed?] (alg-fn data)]
        (if changed?
          [(update data :iterations conj alg-key) true]
          (recur (inc j) algs data)))
      [data false])))

(defn solve-puzzle [puzzle & {:keys [max-iterations] :or {max-iterations 100}}]
  (let [data (assign-values (data/initialize) puzzle)]
    (loop [i 1 data data]
      (let [[data changed?] (run-iteration data)]
        (if (and changed? (< i max-iterations))
          (if (data/data-solved? data)
            (assoc data :solved? true)
            (recur (inc i) data))
          data)))))

;;------------------------------------------------------------------------
;;output functions

(defn print-state [data]
  (let [num-iterations (count (:iterations data))]
    (if (:solved? data)
      (println "Puzzle SOLVED in" num-iterations "iteration(s):" (:iterations data))
      (println "Puzzle NOT solved with" num-iterations "attempt(s)."))))

(defn grid-to-string [wrap-grid wrap-row wrap-done wrap-cell wrap-empty grid]
  (->> (for [i (range 0 9)]
         (->> (for [j (range 0 9)]
                (let [x (+ (* i 9) j)
                      c (get grid x)
                      s (data/cell-contents-to-string c)]
                  (if (data/cell-solved? c)
                    (wrap-done s)
                    (if (data/cell-empty? c)
                      (wrap-empty s)
                      (wrap-cell s)))))
              (reduce str)
              (wrap-row)))
       (reduce str)
       (wrap-grid)))

(defn grid-to-html [grid]
  (grid-to-string (fn [s] (str "<table border=\"1\" cellpadding=\"8\" cellspacing=\"0\">" s "</table>"))
                  (fn [s] (str "<tr>" s "</tr>"))
                  (fn [s] (str "<td align=\"center\"><font color=\"blue\">" s "</font></td>"))
                  (fn [s] (str "<td align=\"center\">" s "</td>"))
                  (fn [s] (str "<td align=\"center\"><font color=\"red\">" s "</font></td>"))
                  grid))

(defn write-string [file-path s]
  (with-open [wtr (clojure.java.io/writer file-path)]
    (.write wtr s)))

(defn write-html [file-path data]
  (write-string file-path (grid-to-html (:grid data))))

;;------------------------------------------------------------------------
