(ns sudoku.core
  (:require [clojure.set :as st]))

;-------------------------------------------------------------------------------
; initialization

(def cell
  (sorted-set 1 2 3 4 5 6 7 8 9))

(def grid
  (vec (repeat 81 cell)))

(defn create-group [xs]
  {
   :cells xs
   ;:set [#{0 1 2 3 4 5 6 7 8}]
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

;-------------------------------------------------------------------------------
; cell functions

(defn remove-cell-value [c val]
  (disj c val))

(defn cell-is-empty [c]
  (= (count c) 0))

(defn cell-is-done [c]
  (= (count c) 1))

(defn cell-contents-to-string [c]
  (if (cell-is-empty c)
    "0"
    (if (cell-is-done c)
      (reduce str c)
      (str "[" (reduce str c) "]"))))

;-------------------------------------------------------------------------------
; grid functions

(defn remove-grid-value [g pos val]
  (assoc g pos
         (remove-cell-value (g pos) val)))

(defn remove-grid-values [grid requests]
  (loop [g grid r requests]
    (if (seq r)
      (let [[pos val] (first r) g (remove-grid-value g pos val)]
        (recur g (next r)))
      g)))

(defn assign-grid-value [g pos val]
  (assoc g pos #{val}))

(defn get-group-cells [grid group]
  (loop [keys (:cells group) acc []]
    (if (seq keys)
      (let [key (first keys)]
        (recur (next keys) (conj acc (grid key))))
      acc)))

;-------------------------------------------------------------------------------
; processing utilities

(defn combos [n x]
  (defn f [p ns x]
    (loop [ns ns acc []]
      (if-let [[n & ns] (seq ns)]
        (let [p (conj p n)
              acc (if (= x 1)
                    (conj acc p)
                    (concat acc (f p ns (dec x))))]
          (recur ns acc))
        acc)))
  (f [] (range 0 n) x))

(defn find-set [cells x]
  (let [cs (combos (count cells) x)]
    (loop [cs cs]
      (if-let [[c & cs] (seq cs)]
        (let [vs (reduce st/union #{} (map (fn [i] (cells i)) c))]
          (if (= (count vs) x)
            [c vs]
            (recur cs)))
        nil))))

(defn find-remove-values [cells rem vs]
  (reduce concat
          (map
           (fn [i]
             (map
              (fn [v] [i v])
              (filter (partial contains? (cells i)) vs)))
           rem)))

(defn process [cells]
  (loop [cells cells
         rem (reduce conj (sorted-set) (range 0 (count cells)))
         x 1
         acc []]
    (if (>= x (count rem)) acc
        (let [cs (map (fn [i] (cells i)) rem)]
          (if-let [[ks vs] (find-set (vec cs) x)]
            (do
              (let [vem (vec rem)
                    rem (reduce disj rem (map (fn [x] (vem x)) ks))
                    zs (find-remove-values cells rem vs)
                    cells (loop [cells cells zs zs]
                            (if-let [[[i v] & zs] (seq zs)]
                              (recur (assoc cells i (disj (cells i) v)) zs)
                              cells))]
                (recur cells rem 1 (concat acc zs))))
            (recur cells rem (inc x) acc))))))

;-------------------------------------------------------------------------------
; simplifying functions

(defn process-naked-singles [cells]
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

(defn compile-num-map [cells]
  (loop [i 0 map {}]
    (if (< i 9)
      (let [cell (cells i)
            map (loop [map map xs (vec cell)]
                  (if (seq xs)
                    (let [x (first xs)
                          ys (conj (get map x []) i)
                          map (assoc map x ys)]
                      (recur map (next xs)))
                    map))]
        (recur (inc i) map))
      map)))

(defn process-hidden-singles [cells]
  (let [num-map (compile-num-map cells)]
    (loop [i 1 acc []]
      (if (<= i 9)
        (let [xs (num-map i)]
          (if  (= (count xs) 1)
            (let [x (first xs)
                  cell (cells x)
                  acc (loop [acc acc nums cell]
                        (if (seq nums)
                          (let [num (first nums)
                                acc (if (not= i num)
                                      (concat acc [[x num]])
                                      acc)]
                            (recur acc (next nums)))
                          acc))]
              (recur (inc i) acc))
            (recur (inc i) acc)))
        acc))))

(defn simplify-group [alg grid group]
  (let [group-keys (:cells group)
        results (alg (get-group-cells grid group))
        updates (mapv (fn [i] (let [[x y] i] [(group-keys x) y])) results)]
    [(remove-grid-values grid updates) (> (count updates) 0)]))

(defn simplify-groups
  ([alg] (fn [data] (simplify-groups alg data)))
  ([alg data]
   (let [groups (:groups data)
         gs (concat (:ver groups) (:hor groups) (:sqr groups))]
     (loop [grid (:grid data)
            gs gs
            changed false]
       (if (seq gs)
         (let [group (first gs)
               [grid group-changed] (simplify-group alg grid group)]
           (recur grid (next gs) (or group-changed changed)))
         [(assoc data :grid grid) changed])))))

(def algorithms
  [
   [(simplify-groups process) "New Naked/Hidden"]
   [(simplify-groups process-naked-singles) "Naked Singles"]
   [(simplify-groups process-hidden-singles) "Hidden Singles"]
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

(defn data-is-solved [data]
  (loop [cells (:grid data)]
    (if (seq cells)
      (let [cell (first cells)]
        (if (cell-is-done cell)
          (recur (next cells))
          false))
      true)))

(defn simplify-data [max-num-runs data]
  (loop [data data counter 1]
    (let [[data changed] (run-algs data counter)]
      (if (and (< counter max-num-runs) (= changed true))
        (if (data-is-solved data)
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

(defn solve-data [data]
  (simplify-data 100 data))

(defn solve-puzzle [puzzle]
  (solve-data (assign-values data puzzle)))

(defn fake-solve-zero-fill [data]
  (let [grid
        (loop [cells (:grid data) counter 0]
          (if (< counter 81)
            (let [cells
                  (if (cell-is-done (cells counter))
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
            (if (cell-is-done c)
              (wrap-done s)
              (if (cell-is-empty c)
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

(defn write-grid [file-path grid]
  (writestr file-path (grid-to-html grid)))

(defn write-data [file-path data]
  (write-grid file-path (:grid data)))

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
