(ns sudoku.algorithms.x-wing
  (:require [clojure.set :as set]
            [sudoku.data :as data]
            [sudoku.util :as util]))

;;------------------------------------------------------------------------

(defn- group-to-value-map ;;invert a grid key to values map to a value to keys map with optional size filter
  ([grid size group]
   (let [mp (group-to-value-map grid group)]
     (->> (filter (fn [[_ vals]] (= (count vals) size)) mp)
          (keys)
          (select-keys mp))))
  ([grid group]
   (->> (select-keys grid (:positions group))
        (map (fn [[key vals]]
               (reduce (fn [mp val]
                         (assoc mp val [key]))
                       {} vals)))
        (apply merge-with concat))))

(defn- x-wing-test [data group-type val i1 i2 j1 j2]
  (let [gmi1 (data/cell-index-to-groups i1)
        gmi2 (data/cell-index-to-groups i2)
        gmj1 (data/cell-index-to-groups j1)
        gmj2 (data/cell-index-to-groups j2)]
    (loop [group-types (disj #{:hor :ver :sqr} group-type)]
      (when-let [[other-group-type & group-types] (seq group-types)]
        (let [group-index-i (get gmi1 other-group-type)
              group-index-j (get gmj1 other-group-type)
              updates (when (and (= group-index-i (get gmi2 other-group-type))
                               (= group-index-j (get gmj2 other-group-type))
                               (not= group-index-i group-index-j))
                        (let [groups (get (:groups data) other-group-type)
                              keys-i (set (:positions (get groups group-index-i)))
                              keys-j (set (:positions (get groups group-index-j)))
                              keys (set/difference (set/union keys-i keys-j) #{i1 i2 j1 j2})]
                          (-> (fn [key]
                                (let [cell (get (:grid data) key)]
                                  (when (contains? cell val)
                                    [[key val]])))
                              (mapcat keys))))]
          (if (= (count updates) 0)
            (recur group-types)
            updates))))))

(defn- x-wing-compare [data group-type m1 m2 keys]
  (loop [keys keys]
    (when-let [[key & keys] (seq keys)]
      (let [[a1 a2] (vec (get m1 key))
            [b1 b2] (vec (get m2 key))
            updates (concat (x-wing-test data group-type key a1 b1 a2 b2)
                            (x-wing-test data group-type key a1 b2 a2 b1))]
        (if (= (count updates) 0)
          (recur keys)
          updates)))))

(defn- x-wing-process-group-type [data group-type]
  (let [grid (:grid data)
        groups (get (:groups data) group-type)
        maps (mapv #(group-to-value-map grid 2 %) groups)]
    (loop [combos (util/generate-range-combinations 9 2)]
      (when-let [[[key1 key2] & combos] (seq combos)]
        (let [m1 (maps key1)
              m2 (maps key2)
              keys (set/intersection (set (keys m1)) (set (keys m2)))
              updates (x-wing-compare data group-type m1 m2 keys)]
          (if (= (count updates) 0)
            (recur combos)
            updates))))))

(defn run-x-wing [data]
  (let [grid (:grid data)]
    (loop [group-types [:hor :ver :sqr]]
      (if-let [[group-type & group-types] (seq group-types)]
        (let [updates (x-wing-process-group-type data group-type)]
          (if (> (count updates) 0)
            [(assoc data :grid (data/remove-values-from-cells grid updates)) true]
            (recur group-types)))
        [data false]))))

;;------------------------------------------------------------------------
