(ns sudoku.core
  (:require
   [clojure.string :as str]
   [clojure.set :as set]))

;;------------------------------------------------------------------------------
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

;;partition 2 sets into their differences and their union
(defn partition-sets [[set1 set2]]
  [(set/difference set1 set2)
   (set/intersection set1 set2)
   (set/difference set2 set1)])

;;------------------------------------------------------------------------------
;;initialization

(def cell
  (sorted-set 1 2 3 4 5 6 7 8 9))

(def starting-positions
  (sorted-set 0 1 2 3 4 5 6 7 8))

(def grid
  (vec (repeat 81 cell)))

(defn create-group [xs]
  {:positions xs
   ;;:unsolved (sorted-set 0 1 2 3 4 5 6 7 8)
   })

(defn create-horz-group [x]
  (let [i (* x 9)
        xs (vec (range i (+ i 9)))]
    (create-group xs)))

(defn create-vert-group [x]
  (let [xs (vec (for [i (range 0 9)] (+ (* i 9)  x)))]
    (create-group xs)))

(defn create-groups [f]
  (vec (for [i (range 0 9)] (f i))))

(def horz-groups (create-groups create-horz-group))
(def vert-groups (create-groups create-vert-group))

(def square-groups
  [(create-group [0 1 2 9 10 11 18 19 20])
   (create-group [3 4 5 12 13 14 21 22 23])
   (create-group [6 7 8 15 16 17 24 25 26])
   (create-group [27 28 29 36 37 38 45 46 47])
   (create-group [30 31 32 39 40 41 48 49 50])
   (create-group [33 34 35 42 43 44 51 52 53])
   (create-group [54 55 56 63 64 65 72 73 74])
   (create-group [57 58 59 66 67 68 75 76 77])
   (create-group [60 61 62 69 70 71 78 79 80])])

(def data
  {:grid grid
   :groups {:ver vert-groups
            :hor horz-groups
            :sqr square-groups}})

(defn get-all-groups [data]
  (let [groups (:groups data)]
    (concat (:ver groups) (:hor groups) (:sqr groups))))

;;------------------------------------------------------------------------------
;;cell functions

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

;;------------------------------------------------------------------------------
;;cells functions

(defn remove-cells-value [cells pos val]
  (assoc cells pos (remove-cell-value (cells pos) val)))

(defn remove-cells-values [cells requests]
  (loop [cells cells rs requests]
    (if-let [[[pos val] & rs] (seq rs)]
      (recur (remove-cells-value cells pos val) rs)
      cells)))

;;------------------------------------------------------------------------------
;;group functions

(defn cell-index-to-groups [i]
  (let [hor (quot i 9)
        ver (mod i 9)
        sqr (+ (* 3 (quot hor 3)) (quot ver 3))]
    {:hor hor :ver ver :sqr sqr}))

(defn other-group-types [group-type]
  (filter
   (fn [gt] (not= group-type gt))
   (keys (:groups data))))

;;------------------------------------------------------------------------------
;;grid functions

(defn assign-grid-value [grid pos val]
  (assoc grid pos #{val}))

(defn get-group-cells [grid group]
  (map grid (:positions group)))

;;------------------------------------------------------------------------------
;;intra-group simplifying

(defn find-matching-set [cells x]
  (loop [combos (generate-combinations (count cells) x)]
    (when-let [[keys & combos] (seq combos)]
      (let [vals (reduce set/union #{} (map (fn [i] (cells i)) keys))]
        (if (= (count vals) x)
          [keys vals]
          (recur combos))))))

(defn find-values-to-remove [cells rems vals]
  (let [contains-val? (fn [i val] (contains? (cells i) val))]
    (reduce concat (map (fn [i]
                          (map
                           (fn [val] [i val])
                           (filter (partial contains-val? i) vals)))
                        rems))))

(defn process-group [cells remaining-positions]
  (loop [cells (vec cells) rems remaining-positions x 1 acc []]
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
  (loop [grid (:grid data) groups (get-all-groups data) changed? false]
    (if-let [[group & groups] (seq groups)]
      (let [[grid group-changed?] (simplify-group grid group)]
        (recur grid groups (or group-changed? changed?)))
      [(assoc data :grid grid) changed?])))

;;------------------------------------------------------------------------------
;;locked candidates

(defn get-locked-group-pairs [data]
  (let [groups (:groups data)]
    (reduce
     concat
     (for [i (range 0 9)]
       (let [sqr (set (:positions ((:sqr groups) i)))]
         (reduce
          concat
          (for [j (range 0 3)]
            [[sqr (set (:positions ((:ver groups) (+ j (* 3 (mod i 3))))))]
             [sqr (set (:positions ((:hor groups) (+ j (* 3 (quot i 3))))))]])))))))

(defn create-unsolved-cell-map [grid positions]
  (loop [xs positions acc {}]
    (if-let [[x & xs] (seq xs)]
      (let [cell (grid x)]
        (if (cell-solved? cell)
          (recur xs acc)
          (recur xs (assoc acc x cell))))
      acc)))

(defn locked-updates-step [val non-vals find-map]
  (when-not (contains? non-vals val)
    (do (defn f [i]
          (let [t (get find-map i)]
            (if (contains? t val) [[i val]] [])))
        (reduce concat (map f (keys find-map))))))

(defn generate-locked-updates [m1 m2 m3]
  (let [[vals1 vals2 vals3] (map (fn [m] (reduce set/union (vals m))) [m1 m2 m3])]
    (loop [vals vals2 acc []]
      (if-let [[val & vals] (seq vals)]
        (let [updates1 (locked-updates-step val vals3 m1)
              updates3 (locked-updates-step val vals1 m3)]
          (recur vals (concat acc updates1 updates3)))
        acc))))

(defn locked-candidates [data]
  (let [grid (:grid data)]
    (loop [pairs (get-locked-group-pairs data)]
      (if-let [[pair & pairs] (seq pairs)]
        (let [[t1 t2 t3] (partition-sets pair)
              [m1 m2 m3] (map (partial create-unsolved-cell-map grid) [t1 t2 t3])
              updates (generate-locked-updates m1 m2 m3)]
          (if (> (count updates) 0)
            [(assoc data :grid (remove-cells-values grid updates)) true]
            (recur pairs )))
        [data false]))))

;;------------------------------------------------------------------------------
;;x-wing

;;invert a grid key to values map to a value to keys map with optional size filter
(defn group-to-value-map
  ([grid size group]
   (let [m (group-to-value-map grid group)]
     (select-keys m (keys (filter (fn [[k v]] (= (count v) size)) m)))))
  ([grid group]
    (apply merge-with concat
           (map
            (fn [[key vals]] (reduce (fn [m val] (assoc m val [key])) {} vals))
            (select-keys grid (:positions group))))))

(defn x-wing-test [data group-type val i1 i2 j1 j2]
  (let [gmi1 (cell-index-to-groups i1)
        gmi2 (cell-index-to-groups i2)
        gmj1 (cell-index-to-groups j1)
        gmj2 (cell-index-to-groups j2)]
    (loop [group-types (other-group-types group-type)]
      (if-let [[other-group-type & group-types] (seq group-types)]
        (let [group-index-i (get gmi1 other-group-type)
              group-index-j (get gmj1 other-group-type)
              updates
              (if (and (= group-index-i (get gmi2 other-group-type))
                       (= group-index-j (get gmj2 other-group-type))
                       (not= group-index-i group-index-j))
                (let [groups (get (:groups data) other-group-type)
                      keys-i (set (:positions (get groups group-index-i)))
                      keys-j (set (:positions (get groups group-index-j)))
                      keys (set/difference (set/union keys-i keys-j) #{i1 i2 j1 j2})]
                  (do
                    (defn f [i]
                      (let [cell (get (:grid data) i)]
                        (if (contains? cell val) [[i val]] [])))
                    (reduce concat (map f keys))))
                [])]
          (if (> (count updates) 0) updates
              (recur group-types)))
        []))))

(defn x-wing-compare [data group-type m1 m2 keys]
  (loop [keys keys]
    (if-let [[key & keys] (seq keys)]
      (let [as (vec (get m1 key))
            a1 (as 0) a2 (as 1)
            bs (vec (get m2 key))
            b1 (bs 0) b2 (bs 1)
            updates (concat
                     (x-wing-test data group-type key a1 b1 a2 b2)
                     (x-wing-test data group-type key a1 b2 a2 b1))]
        (if (> (count updates) 0) updates
            (recur keys)))
      [])))

(defn x-wing-process-group-type [data group-type]
  (let [grid (:grid data)
        groups (get (:groups data) group-type)
        maps (vec (map (partial group-to-value-map grid 2) groups))]
    (loop [combos (generate-combinations 9 2)]
      (if-let [[[k1 k2] & combos] (seq combos)]
        (let [m1 (maps k1) m2 (maps k2)
              keys (set/intersection (set (keys m1)) (set (keys m2)))
              updates (x-wing-compare data group-type m1 m2 keys)]
          (if (> (count updates) 0) updates (recur combos)))
        []))))

(defn x-wing [data]
  (let [grid (:grid data)]
    (loop [group-types [:hor :ver :sqr]]
      (if-let [[group-type & group-types] (seq group-types)]
        (let [updates (x-wing-process-group-type data group-type)]
          (if (> (count updates) 0)
            (do
              ;;(println updates)
              [(assoc data :grid (remove-cells-values grid updates)) true])
            (recur group-types)))
        [data false]))))

;;------------------------------------------------------------------------------
;;algorithms

(def algorithms
  [[simplify-groups "Simplify Groups"]
   [locked-candidates "Locked Candidates"]
   [x-wing "X-Wing"]])

(defn run-algs [data counter]
  (loop [data data algcount 1]
    (let [algorithm (algorithms (dec algcount))
          alg (algorithm 0)
          [data changed?] (alg data)]
      (println (str "Run #" counter " Alg #" algcount " ("
                    (algorithm 1) ") changed=" changed?))
      (if (and (= changed? false) (< algcount (count algorithms)))
        (recur data (inc algcount))
        [data changed?]))))

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
    (let [[data changed?] (run-algs data counter)]
      (if (and (< counter max-num-runs) (= changed? true))
        (if (data-solved? data)
          (do
            (println "Solved!")
            data)
          (recur data (inc counter)))
        data))))

;;------------------------------------------------------------------------------
;;solution functions

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

(defn solve-puzzle [puzzle & {:keys [max-iterations] :or {max-iterations 100}}]
  (solve-data max-iterations (assign-values data puzzle)))

(defn fake-solve-zero-fill [data]
  (let [grid (loop [cells (:grid data) counter 0]
               (if (< counter 81)
                 (let [cells
                       (if (cell-solved? (cells counter))
                         cells
                         (assign-grid-value cells counter 0))]
                   (recur cells (inc counter)))
                 cells))]
    (assoc data :grid grid)))

;;------------------------------------------------------------------------------
;;print functions

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
   (fn [s] (str "<table border=\"1\" cellpadding=\"8\" cellspacing=\"0\">" s "</table>"))
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

;;------------------------------------------------------------------------------
;;puzzles

(def puzzle1
  [[1 1 1]
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
   [9 9 2]])

(def puzzle2
  [[1 2 2]
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
   [9 8 5]])

(def puzzle3
  [[1 2 2]
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
   [9 8 4]])

(def puzzle4
  [[1 4 2]
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
   [9 6 4]])

(def puzzle5
  [[1 5 6]
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
   [9 5 4]])

(def puzzle6
  [[1 1 9]
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
   [9 9 8]])

(def puzzle7
  [[1 1 9]
   [1 5 6]
   [1 6 5]
   [1 7 8]
   [2 1 8]
   [2 3 7]
   [2 9 2]
   [3 5 1]
   [4 1 7]
   [4 3 1]
   [4 4 3]
   [4 8 6]
   [5 2 4]
   [5 8 2]
   [6 2 8]
   [6 6 7]
   [6 7 5]
   [6 9 1]
   [7 5 2]
   [8 1 3]
   [8 7 1]
   [8 9 6]
   [9 3 6]
   [9 4 5]
   [9 5 9]
   [9 9 4]])

(def puzzle8
  [[1 1 9]
   [1 7 4]
   [1 8 8]
   [1 9 6]
   [2 5 9]
   [2 7 1]
   [3 1 2]
   [3 4 5]
   [3 6 6]
   [4 2 1]
   [4 3 6]
   [4 5 3]
   [4 6 5]
   [4 8 4]
   [5 2 2]
   [5 8 6]
   [6 2 9]
   [6 4 6]
   [6 5 8]
   [6 7 2]
   [6 8 3]
   [7 4 9]
   [7 6 8]
   [7 9 4]
   [8 3 2]
   [8 5 5]
   [9 1 6]
   [9 2 4]
   [9 3 9]
   [9 9 8]])

;;locked candidate
(def puzzle9
  [[1 3 7]
   [1 6 5]
   [1 9 3]
   [2 2 4]
   [2 5 2]
   [2 8 9]
   [3 1 1]
   [3 4 8]
   [3 7 6]
   [4 3 8]
   [4 5 6]
   [4 6 9]
   [4 9 2]
   [5 2 6]
   [5 4 2]
   [5 6 1]
   [5 8 3]
   [6 1 4]
   [6 4 5]
   [6 5 8]
   [6 7 1]
   [7 3 6]
   [7 6 8]
   [7 9 1]
   [8 2 2]
   [8 5 1]
   [8 8 8]
   [9 1 8]
   [9 4 7]
   [9 7 3]])

;;locked candidate and x-wing
(def puzzle10
  [[1 2 3]
   [1 3 7]
   [1 4 4]
   [1 5 8]
   [1 6 1]
   [1 7 6]
   [1 9 9]
   [2 2 9]
   [2 5 2]
   [2 6 7]
   [2 8 3]
   [2 9 8]
   [3 1 8]
   [3 4 3]
   [3 6 9]
   [4 2 1]
   [4 3 9]
   [4 4 8]
   [4 5 7]
   [4 6 3]
   [4 8 6]
   [5 1 7]
   [5 2 8]
   [5 6 2]
   [5 8 9]
   [5 9 3]
   [6 4 9]
   [6 6 4]
   [6 7 8]
   [6 8 7]
   [7 4 2]
   [7 5 9]
   [7 6 5]
   [7 8 8]
   [7 9 6]
   [8 3 8]
   [8 4 1]
   [8 5 3]
   [8 6 6]
   [8 7 9]
   [9 1 9]
   [9 2 6]
   [9 3 2]
   [9 4 7]
   [9 5 4]
   [9 6 8]
   [9 7 3]
   [9 8 1]
   [9 9 5]])

;;locked candidate and xy-wing (non-sqr)
(def puzzle11
  [[2 4 1]
   [2 6 7]
   [2 9 8]
   [3 2 7]
   [3 4 3]
   [3 5 9]
   [3 6 2]
   [3 7 5]
   [3 8 4]
   [3 9 1]
   [4 3 4]
   [4 8 9]
   [4 9 2]
   [5 3 5]
   [5 7 6]
   [6 1 9]
   [6 2 3]
   [6 7 4]
   [7 1 1]
   [7 2 9]
   [7 3 2]
   [7 4 7]
   [7 5 8]
   [7 6 5]
   [7 8 6]
   [8 1 5]
   [8 4 4]
   [8 6 3]])

;;xy-wing (sqr)
(def puzzle12
  [[1 5 6]
   [1 7 7]
   [2 1 4]
   [2 6 5]
   [2 7 8]
   [2 9 3]
   [3 3 5]
   [3 6 3]
   [3 8 6]
   [4 2 1]
   [4 6 9]
   [5 3 7]
   [5 5 2]
   [5 7 4]
   [6 4 1]
   [6 8 2]
   [7 2 2]
   [7 4 7]
   [7 7 3]
   [8 1 1]
   [8 3 3]
   [8 4 5]
   [8 9 9]
   [9 3 6]
   [9 5 4]])

;;locked candidates, possible x or xy wing, and swordfish
(def puzzle13
  [[1 3 7]
   [1 7 2]
   [1 8 8]
   [2 3 4]
   [2 5 2]
   [2 6 5]
   [3 1 2]
   [3 2 8]
   [3 6 4]
   [3 7 6]
   [4 2 9]
   [4 6 6]
   [5 1 3]
   [5 9 2]
   [6 4 1]
   [6 8 9]
   [7 3 6]
   [7 4 2]
   [7 8 7]
   [7 9 5]
   [8 4 5]
   [8 5 7]
   [8 7 4]
   [9 2 7]
   [9 3 8]
   [9 7 3]])

;;------------------------------------------------------------------------------
