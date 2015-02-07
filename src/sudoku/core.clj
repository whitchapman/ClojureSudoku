(ns sudoku.core)

;-------------------------------------------------------------------------------
; initialization

(def cell
  (sorted-set 1 2 3 4 5 6 7 8 9))

(def grid
  (vec (repeat 81 cell)))

(defn create_group [xs]
  {:cells xs :set [#{0 1 2 3 4 5 6 7 8}]})

(defn create_horz_group [x]
  (let [i (* x 9)]
    (create_group
     (vec (range i (+ i 9))))))

(defn create_vert_group [x]
  (let [xs
        (vec (for [i (range 0 9)] (+ (* i 9)  x)))]
    (create_group xs)))

(defn create_groups [f]
  (vec (for [i (range 0 9)] (f i))))

(def horz_groups (create_groups create_horz_group))
(def vert_groups (create_groups create_vert_group))

(def square_groups
  [
   (create_group [0 1 2 9 10 11 18 19 20])
   (create_group [3 4 5 12 13 14 21 22 23])
   (create_group [6 7 8 15 16 17 24 25 26])
   (create_group [27 28 29 36 37 38 45 46 47])
   (create_group [30 31 32 39 40 41 48 49 50])
   (create_group [33 34 35 42 43 44 51 52 53])
   (create_group [54 55 56 63 64 65 72 73 74])
   (create_group [57 58 59 66 67 68 75 76 77])
   (create_group [60 61 62 69 70 71 78 79 80])
   ])

(def data
  {
   :grid grid
   :ver vert_groups
   :hor horz_groups
   :sqr square_groups
   })

;-------------------------------------------------------------------------------
; cell functions

(defn remove_cell_value [c val]
  (disj c val))

(defn cell_is_empty [c]
  (= (count c) 0))

(defn cell_is_done [c]
  (= (count c) 1))

(defn cell_contents_to_string [c]
  (if (cell_is_empty c)
    "0"
    (if (cell_is_done c)
      (reduce str c)
      (str "[" (reduce str c) "]")
     ))
  )

;-------------------------------------------------------------------------------
; grid functions

(defn remove_grid_value [g pos val]
  (assoc g pos
         (remove_cell_value (g pos) val)))

(defn remove_grid_values [grid requests]
  (loop [g grid r requests]
    (if (seq r)
      (let [[pos val] (first r) g (remove_grid_value g pos val)]
        (recur g (next r)))
      g)))

(defn assign_grid_value [g pos val]
  (assoc g pos #{val}))

(defn get_group_cells [grid group]
  (loop [keys (:cells group) acc []]
    (if (seq keys)
      (let [key (first keys)]
        (recur (next keys) (conj acc (grid key))))
      acc)))

;-------------------------------------------------------------------------------
; simplifying functions

(defn process_naked_singles [cells]
  (loop [i 0 acc []]
    (if (< i 9)
      (let [r (if (= (count (cells i)) 1)
                (let [v (first (cells i))]
                  (mapv
                   (fn [x] [x v])
                   (filterv
                    (fn [x] (and (not= x i) (contains? (cells x) v)))
                    (range 0 9))))
                [])]
        (recur (inc i) (concat acc r)))
      acc)))

(defn process_cells [cells]
  (process_naked_singles cells))

(defn simplify_group [grid group]
  (let [group_keys (:cells group)
        results (process_cells (get_group_cells grid group))
        updates (mapv (fn [i] (let [[x y] i] [(group_keys x) y])) results)]
    [(remove_grid_values grid updates) (> (count updates) 0)]))

(defn simplify_groups [data]
  (let [groups (concat (:ver data) (:hor data) (:sqr data))]
    (loop [grid (:grid data) groups groups changed false]
      (if (seq groups)
        (let [group (first groups)
              [grid group_changed] (simplify_group grid group)]
          (recur grid (next groups) (or group_changed changed)))
        [(assoc data :grid grid) changed]
        ))))

(defn simplify_data [data]
  (loop [data data counter 1]
    (let [[data changed] (simplify_groups data)]
      (println (str "Run #" counter " changed=" changed))
      (if (and (< counter 10) (= changed true))
        (recur data (inc counter))
        data)
      )))

;-------------------------------------------------------------------------------
; solution functions

(defn fake_solve_zero_fill [data]
  (let [grid
        (loop [cells (:grid data) counter 0]
          (if (< counter 81)
            (let [cells
                  (if (cell_is_done (cells counter))
                    cells
                    (assign_grid_value cells counter 0))]
              (recur cells (inc counter)))
            cells))]
    (assoc data :grid grid)))

(defn data_is_solved [data]
  (loop [cells (:grid data)]
    (if (seq cells)
      (let [cell (first cells)]
        (if (cell_is_done cell)
          (recur (next cells))
          false))
      true)))

;-------------------------------------------------------------------------------
; print functions

(defn grid_to_string [wrap_grid wrap_row wrap_done wrap_cell wrap_empty g]
  (wrap_grid
   (reduce str
    (for [i (range 0 9)]
      (wrap_row
       (reduce
        str
        (for [j (range 0 9)]
          (let [x (+ (* i 9) j) c (g x) s (cell_contents_to_string c)]
            (if (cell_is_done c)
              (wrap_done s)
              (if (cell_is_empty c)
                (wrap_empty s)
                (wrap_cell s)
                ))))))))))

(defn grid_to_html [g]
  (grid_to_string
   (fn [s] (str "<table border=\"1\" cellpadding=\"8\" cellspacing=\"0\">"
               s "</table>"))
   (fn [s] (str "<tr>" s "</tr>"))
   (fn [s] (str "<td align=\"center\"><font color=\"blue\">" s "</font></td>"))
   (fn [s] (str "<td align=\"center\">" s "</td>"))
   (fn [s] (str "<td align=\"center\"><font color=\"red\">" s "</font></td>"))
   g))

(defn writestr [file-path str]
  (with-open [wtr (clojure.java.io/writer file-path)]
    (.write wtr str)))

(defn write_grid [file-path grid]
  (writestr file-path (grid_to_html grid)))

(defn write_data [file-path data]
  (write_grid file-path (:grid data)))

;-------------------------------------------------------------------------------
; puzzles

(defn assign_value [data x y val]
  (let [pos (+ (* (- x 1) 9) (- y 1))]
    (assoc data :grid (assign_grid_value (:grid data) pos val))))

(defn assign_values [data vals]
  (loop [d data vs vals]
    (if (seq vs)
      (let [[x y val] (first vs)
            d (assign_value d x y val)]
       (recur d (next vs)))
      d)
    ))

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

;-------------------------------------------------------------------------------
