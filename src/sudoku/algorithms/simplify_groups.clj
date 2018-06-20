(ns sudoku.algorithms.simplify-groups
  (:require [clojure.set :as set]
            [sudoku.data :as data]
            [sudoku.util :as util]))

;;------------------------------------------------------------------------

(defn- find-matching-set [cells size]
  (loop [combos (util/generate-range-combinations (count cells) size)]
    (when-let [[keys & combos] (seq combos)]
      (let [vals (->> (map #(get cells %) keys)
                      (reduce set/union #{}))]
        (if (= (count vals) size)
          [keys vals]
          (recur combos))))))

(defn- process-group [group-cells]
  (loop [cells (vec group-cells) size 1 remaining-keys (sorted-set 0 1 2 3 4 5 6 7 8) acc []]
    (if (< size (count remaining-keys))
      (let [remaining-cells (mapv (fn [i] (get cells i)) remaining-keys)]
        (if-let [[keys vals] (find-matching-set remaining-cells size)]

          (let [remaining-keys (->> (map #(get (vec remaining-keys) %) keys)
                                    (reduce disj remaining-keys)) ;;remove keys that are in the matching set

                updates (mapcat (fn [key] ;;generate updates that remove vals from remaining cells
                                  (let [cell (get cells key)]
                                    (keep (fn [val]
                                            (when (contains? cell val)
                                              [key val]))
                                          vals)))
                                remaining-keys)]
            (recur (data/remove-values-from-cells cells updates) 1 remaining-keys (concat acc updates)))

          (recur cells (inc size) remaining-keys acc)))
      acc)))

(defn- simplify-group [grid group]
  (let [group-cells (data/get-group-cells grid group)
        positions (:positions group)
        updates (->> (process-group group-cells)
                     (map (fn [[key val]] ;;convert group-cell keys to grid positions
                            (let [pos (get positions key)]
                              [pos val]))))]
    (if (> (count updates) 0)
     [(data/remove-values-from-cells grid updates) true]
     [grid false])))

(defn run-simplify-groups [data]
  (let [reduce-step (fn [[grid grid-changed?] group]
                      (let [[grid group-changed?] (simplify-group grid group)]
                        [grid (or grid-changed? group-changed?)]))]
    (->> (data/get-all-groups data)
         (reduce reduce-step [(:grid data) false])
         ((fn [[grid grid-changed?]] [(assoc data :grid grid) grid-changed?])))))

;;------------------------------------------------------------------------
