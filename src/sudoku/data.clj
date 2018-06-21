(ns sudoku.data
  (:require [clojure.set :as set]))

;;------------------------------------------------------------------------
;;cells

(defn cell-empty? [c]
  (= (count c) 0))

(defn cell-solved? [c]
  (= (count c) 1))

(defn cell-contents-to-string [c]
  (if-not (cell-empty? c)
    (if (cell-solved? c)
      (reduce str c)
      (str "[" (reduce str c) "]"))
    "0"))

(defn remove-values-from-cells [cells pos-val-pairs]
  (reduce (fn [cells [pos val]]
            (let [updated-cell (disj (get cells pos) val)]
              (assoc cells pos updated-cell)))
          cells pos-val-pairs))

;;------------------------------------------------------------------------
;;groups

(defn create-group [xs]
  {:positions (vec xs)
   ;;:unsolved (sorted-set 0 1 2 3 4 5 6 7 8) ;;TODO: possible optimization
   })

(defn create-horz-group [x]
  (let [i (* x 9)]
    (->> (range 0 9)
         (map #(+ i %))
         (create-group))))

(defn create-vert-group [x]
  (->> (range 0 9)
       (map #(+ x (* 9 %)))
       (create-group)))

(defn create-groups [f]
  (->> (range 0 9)
       (map f)
       (vec)))

(def groups
  {:ver (create-groups create-vert-group)
   :hor (create-groups create-horz-group)
   :sqr [(create-group [0 1 2 9 10 11 18 19 20])
         (create-group [3 4 5 12 13 14 21 22 23])
         (create-group [6 7 8 15 16 17 24 25 26])
         (create-group [27 28 29 36 37 38 45 46 47])
         (create-group [30 31 32 39 40 41 48 49 50])
         (create-group [33 34 35 42 43 44 51 52 53])
         (create-group [54 55 56 63 64 65 72 73 74])
         (create-group [57 58 59 66 67 68 75 76 77])
         (create-group [60 61 62 69 70 71 78 79 80])]})

;;------------------------------------------------------------------------
;;grid

(defn assign-grid-value [grid pos val]
  (assoc grid pos #{val}))

(defn get-group-cells [grid group]
  (map grid (:positions group)))

(defn cell-position-to-group-indexes [position]
  (let [hor-group-index (quot position 9)
        ver-group-index (mod position 9)
        sqr-group-index (+ (* 3 (quot hor-group-index 3)) (quot ver-group-index 3))]
    {:hor hor-group-index :ver ver-group-index :sqr sqr-group-index}))

(defn get-related-positions [position]
  (let [{:keys [hor ver sqr]} (cell-position-to-group-indexes position)]
    (-> (set/union (set (:positions (get (:hor groups) hor)))
                   (set (:positions (get (:ver groups) ver)))
                   (set (:positions (get (:sqr groups) sqr))))
        (disj position))))

;;------------------------------------------------------------------------
;;intialization

(defn initialize []
  (let [cell (sorted-set 1 2 3 4 5 6 7 8 9)]
    {:grid (vec (repeat 81 cell))
     :groups groups
     :iterations []
     :solved? false}))

;;------------------------------------------------------------------------
;;data

(defn get-all-groups [data]
  (let [groups (:groups data)]
    (concat (:ver groups) (:hor groups) (:sqr groups))))

(defn data-solved? [data]
  (loop [cells (:grid data)]
    (if-let [[cell & cells] (seq cells)]
      (if (cell-solved? cell)
        (recur cells)
        false)
      true)))

;;------------------------------------------------------------------------
