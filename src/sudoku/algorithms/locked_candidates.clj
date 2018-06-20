(ns sudoku.algorithms.locked-candidates
  (:require [clojure.set :as set]
            [sudoku.data :as data]))

;;------------------------------------------------------------------------

(defn- get-locked-group-pairs [data]
  (let [groups (:groups data)]
    (->> (range 9)
         (mapcat (fn [i]
                   (let [sqr (set (:positions (get (:sqr groups) i)))
                         i-ver (* 3 (mod i 3))
                         i-hor (* 3 (quot i 3))]
                     (->> (range 3)
                          (mapcat (fn [j] [[sqr (set (:positions (get (:ver groups) (+ j i-ver))))]
                                          [sqr (set (:positions (get (:hor groups) (+ j i-hor))))]])))))))))

(defn- create-unsolved-cell-map [grid positions]
  (loop [positions positions acc {}]
    (if-let [[pos & positions] (seq positions)]
      (let [cell (get grid pos)]
        (if (data/cell-solved? cell)
          (recur positions acc)
          (recur positions (assoc acc pos cell))))
      acc)))

(defn- locked-updates-step [val non-vals find-map]
  (when-not (contains? non-vals val)
    (mapcat (fn [i]
              (let [t (get find-map i)]
                (if (contains? t val)
                  [[i val]] [])))
            (keys find-map))))

(defn- generate-locked-updates [m1 m2 m3]
  (let [[vals1 vals2 vals3] (map (fn [m] (reduce set/union (vals m))) [m1 m2 m3])]
    (loop [vals vals2 acc []]
      (if-let [[val & vals] (seq vals)]
        (let [updates1 (locked-updates-step val vals3 m1)
              updates3 (locked-updates-step val vals1 m3)]
          (recur vals (concat acc updates1 updates3)))
        acc))))

(defn run-locked-candidates [data]
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
            [(assoc data :grid (data/remove-values-from-cells grid updates)) true]
            (recur pairs )))

        [data false]))))

;;------------------------------------------------------------------------
