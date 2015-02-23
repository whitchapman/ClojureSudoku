(ns sudoku.core)

;-------------------------------------------------------------------------------
; initialization

(def cell
  (sorted-set 1 2 3 4 5 6 7 8 9))

(def starting-positions
  (sorted-set 0 1 2 3 4 5 6 7 8))

(def grid
  (vec (repeat 81 cell)))

(defn create-group [xs]
  {
   :positions xs
   ;:unsolved (sorted-set 0 1 2 3 4 5 6 7 8)
   }
  )

(defn create-horz-group [x]
  (let [i (* x 9)]
    (create-group
     (vec (range i (+ i 9))))))

(defn create-vert-group [x]
  (let [xs
        (vec (for [i (range 0 9)] (+ (* i 9)  x)))]
    (create-group xs)))

(defn create-groups [f]
  (vec (for [i (range 0 9)] (f i))))

(def horz-groups (create-groups create-horz-group))
(def vert-groups (create-groups create-vert-group))

(def square-groups
  [
   (create-group [0 1 2 9 10 11 18 19 20])
   (create-group [3 4 5 12 13 14 21 22 23])
   (create-group [6 7 8 15 16 17 24 25 26])
   (create-group [27 28 29 36 37 38 45 46 47])
   (create-group [30 31 32 39 40 41 48 49 50])
   (create-group [33 34 35 42 43 44 51 52 53])
   (create-group [54 55 56 63 64 65 72 73 74])
   (create-group [57 58 59 66 67 68 75 76 77])
   (create-group [60 61 62 69 70 71 78 79 80])
   ])

(def data
  {
   :grid grid
   :groups {
            :ver vert-groups
            :hor horz-groups
            :sqr square-groups
            }
   })

(defn get-all-groups [data]
  (let [groups (:groups data)]
    (concat (:ver groups) (:hor groups) (:sqr groups))))

;-------------------------------------------------------------------------------
; cell functions

(defn cell-empty? [c]
  (= (count c) 0))

(defn cell-solved? [c]
  (= (count c) 1))

(defn cell-contents-to-string [c]
  (if (cell-empty? c) "0"
      (if (cell-solved? c)
        (reduce str c)
        (str "[" (reduce str c) "]"))))

(defn remove-cell-value [c val]
  (disj c val))

;-------------------------------------------------------------------------------
; cells functions

(defn remove-cells-value [cells pos val]
  (assoc cells pos (remove-cell-value (cells pos) val)))

(defn remove-cells-values [cells requests]
  (loop [cells cells rs requests]
    (if-let [[[pos val] & rs] (seq rs)]
      (recur (remove-cells-value cells pos val) rs)
      cells)))

;-------------------------------------------------------------------------------
; grid functions

(defn assign-grid-value [grid pos val]
  (assoc grid pos #{val}))

(defn get-group-cells [grid group]
  (loop [keys (:positions group) acc []]
    (if-let [[key & keys] (seq keys)]
      (recur keys (conj acc (grid key)))
      acc)))

;-------------------------------------------------------------------------------
; intra-group simplifying

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

(defn find-matching-set [cells x]
  (loop [combos (generate-combinations (count cells) x)]
    (if-let [[keys & combos] (seq combos)]
      (let [vals (reduce clojure.set/union #{} (map (fn [i] (cells i)) keys))]
        (if (= (count vals) x)
          [keys vals]
          (recur combos)))
      nil)))

(defn find-values-to-remove [cells rems vals]
  (let [contains-val? (fn [i val] (contains? (cells i) val))]
    (reduce concat (map (fn [i]
                          (map
                           (fn [val] [i val])
                           (filter (partial contains-val? i) vals)))
                        rems))))

(defn process-group [cells remaining-positions]
  (loop [cells cells rems remaining-positions x 1 acc []]
    (if (>= x (count rems)) acc
        (let [cs (map (fn [i] (cells i)) rems)]
          (if-let [[keys vals] (find-matching-set (vec cs) x)]
            (let [vrems (vec rems)
                  rems (reduce disj rems (map (fn [k] (vrems k)) keys))
                  results (find-values-to-remove cells rems vals)
                  cells (remove-cells-values cells results)]
              (recur cells rems 1 (concat acc results)))
            (recur cells rems (inc x) acc))))))

(defn simplify-group [grid group]
  (let [cells (get-group-cells grid group)
        results (process-group cells starting-positions)
        group-keys (:positions group)
        updates (mapv (fn [i] (let [[x y] i] [(group-keys x) y])) results)]
    [(remove-cells-values grid updates) (> (count updates) 0)]))

(defn simplify-groups [data]
  (loop [grid (:grid data) groups (get-all-groups data) changed false]
    (if-let [[group & groups] (seq groups)]
      (let [[grid group-changed] (simplify-group grid group)]
        (recur grid groups (or group-changed changed)))
      [(assoc data :grid grid) changed])))

;-------------------------------------------------------------------------------
; algorithms

(def algorithms
  [
   [simplify-groups "Simplify Groups"]
   ])

(defn run-algs [data counter]
  (loop [data data algcount 1]
    (let [algorithm (algorithms (dec algcount))
          alg (algorithm 0)
          [data changed] (alg data)]
      (println (str "Run #" counter " Alg #" algcount " ("
                    (algorithm 1) ") changed=" changed))
      (if (and (= changed false) (< algcount (count algorithms)))
        (recur data (inc algcount))
        [data changed]
        ))))

(defn data-solved? [data]
  (loop [cells (:grid data)]
    (if (seq cells)
      (let [cell (first cells)]
        (if (cell-solved? cell)
          (recur (next cells))
          false))
      true)))

(defn solve-data [max-num-runs data]
  (loop [data data counter 1]
    (let [[data changed] (run-algs data counter)]
      (if (and (< counter max-num-runs) (= changed true))
        (if (data-solved? data)
          (do
            (println "Solved!")
            data)
          (recur data (inc counter)))
        data))))

;-------------------------------------------------------------------------------
; solution functions

(defn assign-value [data x y val]
  (let [pos (+ (* (- x 1) 9) (- y 1))]
    (assoc data :grid (assign-grid-value (:grid data) pos val))))

(defn assign-values [data vals]
  (loop [d data vs vals]
    (if (seq vs)
      (let [[x y val] (first vs)
            d (assign-value d x y val)]
       (recur d (next vs)))
      d)))

(defn solve-puzzle
  ([puzzle] (solve-puzzle puzzle 100))
  ([puzzle max-iterations]
   (solve-data max-iterations (assign-values data puzzle))))

(defn fake-solve-zero-fill [data]
  (let [grid
        (loop [cells (:grid data) counter 0]
          (if (< counter 81)
            (let [cells
                  (if (cell-solved? (cells counter))
                    cells
                    (assign-grid-value cells counter 0))]
              (recur cells (inc counter)))
            cells))]
    (assoc data :grid grid)))

;-------------------------------------------------------------------------------
; print functions

(defn grid-to-string [wrap-grid wrap-row wrap-done wrap-cell wrap-empty g]
  (wrap-grid
   (reduce str
    (for [i (range 0 9)]
      (wrap-row
       (reduce
        str
        (for [j (range 0 9)]
          (let [x (+ (* i 9) j) c (g x) s (cell-contents-to-string c)]
            (if (cell-solved? c)
              (wrap-done s)
              (if (cell-empty? c)
                (wrap-empty s)
                (wrap-cell s)))))))))))

(defn grid-to-html [grid]
  (grid-to-string
   (fn [s] (str "<table border=\"1\" cellpadding=\"8\" cellspacing=\"0\">"
               s "</table>"))
   (fn [s] (str "<tr>" s "</tr>"))
   (fn [s] (str "<td align=\"center\"><font color=\"blue\">" s "</font></td>"))
   (fn [s] (str "<td align=\"center\">" s "</td>"))
   (fn [s] (str "<td align=\"center\"><font color=\"red\">" s "</font></td>"))
   grid))

(defn writestr [file-path str]
  (with-open [wtr (clojure.java.io/writer file-path)]
    (.write wtr str)))

(defn write-data [file-path data]
  (writestr file-path (grid-to-html (:grid data))))

;-------------------------------------------------------------------------------
; puzzles

(def puzzle1
  [
   [1 1 1]
   [1 4 9]
   [1 6 2]
   [1 8 3]
   [2 2 3]
   [2 5 8]
   [3 2 9]
   [3 3 6]
   [3 4 7]
   [3 7 2]
   [3 8 1]
   [4 1 8]
   [4 2 6]
   [4 3 2]
   [4 7 9]
   [5 2 1]
   [5 8 8]
   [6 3 4]
   [6 7 6]
   [6 8 2]
   [6 9 1]
   [7 2 2]
   [7 3 1]
   [7 6 6]
   [7 7 3]
   [7 8 9]
   [8 5 2]
   [8 8 5]
   [9 2 4]
   [9 4 1]
   [9 6 7]
   [9 9 2]
   ])

(def puzzle2
  [
   [1 2 2]
   [1 3 5]
   [1 4 1]
   [1 5 9]
   [2 1 6]
   [2 4 3]
   [2 6 8]
   [2 7 1]
   [3 1 1]
   [3 5 2]
   [3 8 9]
   [4 1 9]
   [4 3 6]
   [4 8 3]
   [6 2 1]
   [6 7 6]
   [6 9 5]
   [7 2 9]
   [7 5 4]
   [7 9 6]
   [8 3 4]
   [8 4 9]
   [8 6 5]
   [8 9 2]
   [9 5 8]
   [9 6 2]
   [9 7 4]
   [9 8 5]
   ])

(def puzzle3
  [
   [1 2 2]
   [1 7 6]
   [1 8 1]
   [2 3 8]
   [2 6 1]
   [2 8 5]
   [2 9 3]
   [3 5 7]
   [4 2 9]
   [4 3 5]
   [4 4 4]
   [4 7 1]
   [5 4 8]
   [5 6 9]
   [6 3 6]
   [6 6 5]
   [6 7 8]
   [6 8 2]
   [7 5 4]
   [8 1 6]
   [8 2 5]
   [8 4 2]
   [8 7 7]
   [9 2 3]
   [9 3 9]
   [9 8 4]
   ])

(def puzzle4
  [
   [1 4 2]
   [1 7 8]
   [1 8 3]
   [2 5 8]
   [2 7 5]
   [3 1 3]
   [3 2 8]
   [3 4 5]
   [3 7 6]
   [4 3 1]
   [4 8 9]
   [4 9 3]
   [5 3 4]
   [5 7 1]
   [6 1 9]
   [6 2 6]
   [6 7 2]
   [7 3 3]
   [7 6 9]
   [7 8 1]
   [7 9 4]
   [8 3 9]
   [8 5 5]
   [9 2 1]
   [9 3 2]
   [9 6 4]
   ])

(def puzzle5
  [
   [1 5 6]
   [1 6 2]
   [2 4 3]
   [2 7 4]
   [3 1 2]
   [3 3 3]
   [3 7 1]
   [3 8 9]
   [4 1 8]
   [4 4 9]
   [4 8 4]
   [5 1 6]
   [5 5 5]
   [5 9 8]
   [6 2 4]
   [6 6 3]
   [6 9 2]
   [7 2 5]
   [7 3 4]
   [7 7 8]
   [7 9 1]
   [8 3 7]
   [8 6 5]
   [9 4 1]
   [9 5 4]
   ])

(def puzzle6
  [
   [1 1 9]
   [1 5 1]
   [1 9 7]
   [2 4 4]
   [2 6 9]
   [2 7 6]
   [2 8 1]
   [3 3 4]
   [4 1 6]
   [4 2 5]
   [4 6 7]
   [4 7 9]
   [5 5 4]
   [6 3 1]
   [6 4 2]
   [6 8 8]
   [6 9 5]
   [7 7 2]
   [8 2 1]
   [8 3 6]
   [8 4 9]
   [8 6 5]
   [9 1 5]
   [9 5 3]
   [9 9 8]
   ])

;-------------------------------------------------------------------------------
