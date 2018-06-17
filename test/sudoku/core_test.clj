(ns sudoku.core-test
  (:require [clojure.test :refer :all]
            [sudoku.core :refer :all]))

(deftest test-puzzle1
  (testing "how many iterations to solve puzzle1?"
    (let [data (solve-puzzle puzzle1 :max-iterations 1)]
      (is (= false (data-solved? data))))

    (let [data (solve-puzzle puzzle1 :max-iterations 2)]
      (is (= false (data-solved? data))))

    (let [data (solve-puzzle puzzle1)]
      (is (= true (data-solved? data))))))
