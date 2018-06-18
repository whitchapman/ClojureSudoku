(ns sudoku.algorithms
  (:require [clojure.set :as set]
            [sudoku.data :as data]
            [sudoku.util :as util]))

;;------------------------------------------------------------------------
;;intra-group simplifying

(defn- find-matching-set [cells x]
  (loop [combos (util/generate-combinations (count cells) x)]
    (when-let [[keys & combos] (seq combos)]
      (let [vals (reduce set/union #{} (map (fn [i] (get cells i)) keys))]
        (if (= (count vals) x)
          [keys vals]
          (recur combos))))))

(defn- find-values-to-remove [cells rems vals]
  (let [contains-val? (fn [i val] (contains? (get cells i) val))]
    (reduce concat (map (fn [i]
                          (map
                           (fn [val] [i val])
                           (filter (partial contains-val? i) vals)))
                        rems))))

(defn- process-group [cells remaining-positions]
  (loop [cells (vec cells) rems remaining-positions x 1 acc []]
    (if (>= x (count rems)) acc
        (let [cs (map (fn [i] (get cells i)) rems)]
          (if-let [[keys vals] (find-matching-set (vec cs) x)]
            (let [vrems (vec rems)
                  rems (reduce disj rems (map (fn [k] (vrems k)) keys))
                  results (find-values-to-remove cells rems vals)
                  cells (data/remove-cells-values cells results)]
              (recur cells rems 1 (concat acc results)))
            (recur cells rems (inc x) acc))))))

(defn- simplify-group [grid group]
  (let [cells (data/get-group-cells grid group)
        results (process-group cells (sorted-set 0 1 2 3 4 5 6 7 8))
        group-keys (:positions group)
        updates (mapv (fn [i] (let [[x y] i] [(group-keys x) y])) results)]
    [(data/remove-cells-values grid updates) (> (count updates) 0)]))

(defn simplify-groups [data]
  (loop [grid (:grid data) groups (data/get-all-groups data) changed? false]
    (if-let [[group & groups] (seq groups)]
      (let [[grid group-changed?] (simplify-group grid group)]
        (recur grid groups (or group-changed? changed?)))
      [(assoc data :grid grid) changed?])))

;;------------------------------------------------------------------------
;;locked candidates

(defn- get-locked-group-pairs [data]
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

(defn- create-unsolved-cell-map [grid positions]
  (loop [xs positions acc {}]
    (if-let [[x & xs] (seq xs)]
      (let [cell (grid x)]
        (if (data/cell-solved? cell)
          (recur xs acc)
          (recur xs (assoc acc x cell))))
      acc)))

(defn- locked-updates-step [val non-vals find-map]
  (when-not (contains? non-vals val)
    (do (defn f [i]
          (let [t (get find-map i)]
            (if (contains? t val) [[i val]] [])))
        (reduce concat (map f (keys find-map))))))

(defn- generate-locked-updates [m1 m2 m3]
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
        (let [[s1 s2] pair

              ;;partition 2 sets into their differences and their intersection
              m1 (create-unsolved-cell-map grid (set/difference s1 s2))
              m2 (create-unsolved-cell-map grid (set/intersection s1 s2))
              m3 (create-unsolved-cell-map grid (set/difference s2 s1))

              updates (generate-locked-updates m1 m2 m3)]
          (if (> (count updates) 0)
            [(assoc data :grid (data/remove-cells-values grid updates)) true]
            (recur pairs )))
        [data false]))))

;;------------------------------------------------------------------------
;;x-wing

;;invert a grid key to values map to a value to keys map with optional size filter
(defn- group-to-value-map
  ([grid size group]
   (let [m (group-to-value-map grid group)]
     (select-keys m (keys (filter (fn [[k v]] (= (count v) size)) m)))))
  ([grid group]
    (apply merge-with concat
           (map
            (fn [[key vals]] (reduce (fn [m val] (assoc m val [key])) {} vals))
            (select-keys grid (:positions group))))))

(defn- x-wing-test [data group-type val i1 i2 j1 j2]
  (let [gmi1 (data/cell-index-to-groups i1)
        gmi2 (data/cell-index-to-groups i2)
        gmj1 (data/cell-index-to-groups j1)
        gmj2 (data/cell-index-to-groups j2)]
    (loop [group-types (data/other-group-types data group-type)]
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

(defn- x-wing-compare [data group-type m1 m2 keys]
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

(defn- x-wing-process-group-type [data group-type]
  (let [grid (:grid data)
        groups (get (:groups data) group-type)
        maps (vec (map (partial group-to-value-map grid 2) groups))]
    (loop [combos (util/generate-combinations 9 2)]
      (if-let [[[k1 k2] & combos] (seq combos)]
        (let [m1 (maps k1) m2 (maps k2)
              keys (set/intersection (set (keys m1)) (set (keys m2)))
              updates (x-wing-compare data group-type m1 m2 keys)]
          (if (> (count updates) 0)
            updates
            (recur combos)))
        []))))

(defn x-wing [data]
  (let [grid (:grid data)]
    (loop [group-types [:hor :ver :sqr]]
      (if-let [[group-type & group-types] (seq group-types)]
        (let [updates (x-wing-process-group-type data group-type)]
          (if (> (count updates) 0)
            [(assoc data :grid (data/remove-cells-values grid updates)) true]
            (recur group-types)))
        [data false]))))

;;------------------------------------------------------------------------
