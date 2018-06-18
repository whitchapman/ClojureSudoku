(ns sudoku.core
  (:require [sudoku.algorithms :as algorithms]
            [sudoku.data :as data]
            [sudoku.puzzles :as puzzles]))

;;------------------------------------------------------------------------
;;assignment

(defn- assign-value [data [x y val]]
  (let [pos (+ (* (- x 1) 9) (- y 1))]
    (assoc data :grid (data/assign-grid-value (:grid data) pos val))))

(defn assign-values [data vs]
  (loop [data data vs vs]
    (if-let [[v & vs] (seq vs)]
      (let [data (assign-value data v)]
        (recur data vs))
      data)))

(defn fake-solve-zero-fill [data]
  (let [grid (loop [cells (:grid data) counter 0]
               (if (< counter 81)
                 (let [cells (if (data/cell-solved? (get cells counter))
                               cells
                               (data/assign-grid-value cells counter 0))]
                   (recur cells (inc counter)))
                 cells))]
    (assoc data :grid grid)))

;;------------------------------------------------------------------------
;;solution functions

(def algorithms
  [[:simplify-groups algorithms/simplify-groups]
   [:locked-candidates algorithms/locked-candidates]
   [:x-wing algorithms/x-wing]])

(defn run-iteration [data & {:keys [algs] :or {algs algorithms}}]
  (loop [j 1 algs algs data data]
    (if-let [[[alg-key alg-fn] & algs] (seq algs)]
      (let [[data changed?] (alg-fn data)]
        ;;(println (str "  Alg #" j " (" alg-key ") changed=" changed?))
        (if changed?
          [(update data :iterations conj alg-key) true]
          (recur (inc j) algs data)))
      [data false])))

(defn solve-puzzle [puzzle & {:keys [max-iterations] :or {max-iterations 100}}]
  ;;(println "")
  (let [data (assign-values (data/initialize) puzzle)]
    (loop [i 1 data data]
      ;;(println (str "Iteration #" i ":"))
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

(defn grid-to-string [wrap-grid wrap-row wrap-done wrap-cell wrap-empty g]
  (wrap-grid
   (reduce str
    (for [i (range 0 9)]
      (wrap-row
       (reduce
        str
        (for [j (range 0 9)]
          (let [x (+ (* i 9) j) c (g x) s (data/cell-contents-to-string c)]
            (if (data/cell-solved? c)
              (wrap-done s)
              (if (data/cell-empty? c)
                (wrap-empty s)
                (wrap-cell s)))))))))))

(defn grid-to-html [grid]
  (grid-to-string (fn [s] (str "<table border=\"1\" cellpadding=\"8\" cellspacing=\"0\">" s "</table>"))
                  (fn [s] (str "<tr>" s "</tr>"))
                  (fn [s] (str "<td align=\"center\"><font color=\"blue\">" s "</font></td>"))
                  (fn [s] (str "<td align=\"center\">" s "</td>"))
                  (fn [s] (str "<td align=\"center\"><font color=\"red\">" s "</font></td>"))
                  grid))

(defn write-string [file-path str]
  (with-open [wtr (clojure.java.io/writer file-path)]
    (.write wtr str)))

(defn write-html [file-path data]
  (write-string file-path (grid-to-html (:grid data))))

;;------------------------------------------------------------------------
