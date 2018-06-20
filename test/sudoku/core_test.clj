(ns sudoku.core-test
  (:require [clojure.test :refer :all]
            [sudoku.core :refer :all]
            [sudoku.data :as data]
            [sudoku.puzzles :as puzzles]
            [sudoku.util :as util]))

(deftest test-combinations

  (is (= [[0]] (util/generate-range-combinations 1 1)))

  (is (= [[0 1]
          [0 2]
          [0 3]
          [1 2]
          [1 3]
          [2 3]]
         (util/generate-range-combinations 4 2)))

  (is (= [[0 1 2]
          [0 1 3]
          [0 1 4]
          [0 2 3]
          [0 2 4]
          [0 3 4]
          [1 2 3]
          [1 2 4]
          [1 3 4]
          [2 3 4]]
         (util/generate-range-combinations 5 3)))

  (is (= [[0 1 2 3]] (util/generate-range-combinations 4 4))))

(deftest test-zero-fille
  (let [data (fake-solve-zero-fill (data/initialize))]
    (is (= (repeat 81 #{0}) (:grid data)))))

(deftest test-puzzle1
  (let [data (data/initialize)]
    (is (= 0 (count (:iterations data))))
    (is (= false (:solved? data)))

    (let [data (assign-values data puzzles/puzzle1)]
      (is (= 0 (count (:iterations data))))
      (is (= false (:solved? data)))

      (let [data1 (run-iteration data)]
        (is (not= nil data1))
        (is (= 1 (count (:iterations data1))))
        (is (= false (data/data-solved? data1)))

        (let [data2 (run-iteration data1)]
          (is (not= nil data2))
          (is (= 2 (count (:iterations data2))))
          (is (= false (data/data-solved? data2)))

          (let [data3 (run-iteration data2)]
            (is (not= nil data3))
            (is (= 3 (count (:iterations data3))))
            (is (= true (data/data-solved? data3)))

            (let [data4 (run-iteration data3)]
              (is (= nil data4))))))))

  (let [data (solve-puzzle puzzles/puzzle1 :max-iterations 1)]
    (is (= false (:solved? data))))

  (let [data (solve-puzzle puzzles/puzzle1 :max-iterations 2)]
    (is (= false (:solved? data))))

  (let [data (solve-puzzle puzzles/puzzle1)]
    (is (= 3 (count (:iterations data))))
    (is (= #{:simplify-groups} (set (:iterations data))))
    (is (= true (:solved? data)))))

(deftest test-puzzle2
  (let [data (solve-puzzle puzzles/puzzle2)]
    (is (= 7 (count (:iterations data))))
    (is (= #{:simplify-groups} (set (:iterations data))))
    (is (= true (:solved? data)))))

(deftest test-puzzle3
  (let [data (solve-puzzle puzzles/puzzle3)]
    (is (= 5 (count (:iterations data))))
    (is (= #{:simplify-groups} (set (:iterations data))))
    (is (= true (:solved? data)))))

(deftest test-puzzle4
  (let [data (solve-puzzle puzzles/puzzle4)]
    (is (= 5 (count (:iterations data))))
    (is (= #{:simplify-groups} (set (:iterations data))))
    (is (= true (:solved? data)))))

(deftest test-puzzle5
  (let [data (solve-puzzle puzzles/puzzle5)]
    (is (= 9 (count (:iterations data))))
    (is (= #{:simplify-groups} (set (:iterations data))))
    (is (= true (:solved? data)))))

(deftest test-puzzle6
  (let [data (solve-puzzle puzzles/puzzle6)]
    (is (= 6 (count (:iterations data))))
    (is (= #{:simplify-groups} (set (:iterations data))))
    (is (= true (:solved? data)))))

(deftest test-puzzle7
  (let [data (solve-puzzle puzzles/puzzle7)]
    (is (= 6 (count (:iterations data))))
    (is (= #{:simplify-groups} (set (:iterations data))))
    (is (= true (:solved? data)))))

(deftest test-puzzle8
  (let [data (solve-puzzle puzzles/puzzle8)]
    (is (= 6 (count (:iterations data))))
    (is (= #{:simplify-groups} (set (:iterations data))))
    (is (= true (:solved? data)))))

(deftest test-puzzle9
  (let [data (solve-puzzle puzzles/puzzle9)]
    (is (= 9 (count (:iterations data))))
    (is (= #{:simplify-groups :locked-candidates} (set (:iterations data))))
    (is (= true (:solved? data)))))

(deftest test-puzzle10
  (let [data (solve-puzzle puzzles/puzzle10)]
    (is (= 8 (count (:iterations data))))
    (is (= #{:simplify-groups :locked-candidates :x-wing} (set (:iterations data))))
    (is (= true (:solved? data)))))

(deftest test-puzzles-unsolvable
  (let [data (solve-puzzle puzzles/puzzle11)]
    (is (= 7 (count (:iterations data))))
    (is (= #{:simplify-groups :locked-candidates} (set (:iterations data))))
    (is (= false (:solved? data))))

  (let [data (solve-puzzle puzzles/puzzle12)]
    (is (= 7 (count (:iterations data))))
    (is (= #{:simplify-groups} (set (:iterations data))))
    (is (= false (:solved? data))))

  (let [data (solve-puzzle puzzles/puzzle13)]
    (is (= 5 (count (:iterations data))))
    (is (= #{:simplify-groups :locked-candidates} (set (:iterations data))))
    (is (= false (:solved? data)))))
