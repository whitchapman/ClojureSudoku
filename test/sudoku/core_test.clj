(ns sudoku.core-test
  (:require [clojure.test :refer :all]
            [sudoku.core :refer :all]
            [sudoku.puzzles :as puzzles]))

(deftest test-puzzles
  (let [data (solve-puzzle puzzles/puzzle1 :max-iterations 1)]
    (is (= false (data-solved? data))))

  (let [data (solve-puzzle puzzles/puzzle1 :max-iterations 2)]
    (is (= false (data-solved? data))))

  (let [data (solve-puzzle puzzles/puzzle1)]
    (is (= true (data-solved? data))))

  (let [data (solve-puzzle puzzles/puzzle2)]
    (is (= true (data-solved? data))))

  (let [data (solve-puzzle puzzles/puzzle3)]
    (is (= true (data-solved? data))))

  (let [data (solve-puzzle puzzles/puzzle4)]
    (is (= true (data-solved? data))))

  (let [data (solve-puzzle puzzles/puzzle5)]
    (is (= true (data-solved? data))))

  (let [data (solve-puzzle puzzles/puzzle6)]
    (is (= true (data-solved? data))))

  (let [data (solve-puzzle puzzles/puzzle7)]
    (is (= true (data-solved? data))))

  (let [data (solve-puzzle puzzles/puzzle8)]
    (is (= true (data-solved? data))))

  (let [data (solve-puzzle puzzles/puzzle9)]
    (is (= true (data-solved? data))))

  (let [data (solve-puzzle puzzles/puzzle10)]
    (is (= true (data-solved? data))))


  (let [data (solve-puzzle puzzles/puzzle11)]
    (is (= false (data-solved? data))))

  (let [data (solve-puzzle puzzles/puzzle12)]
    (is (= false (data-solved? data))))

  (let [data (solve-puzzle puzzles/puzzle13)]
    (is (= false (data-solved? data))))

  )
