(ns sudoku-clojure.core-test
    (:require
     [cljs.test :refer-macros [deftest is testing]]
     [sudoku-clojure.sudoku :as sudoku]
     [sudoku-clojure.core :refer [multiply]]))

(deftest choice-test
  (and
   (is (= (sudoku/choice 0) [1 2 3 4 5 6 7 8 9]))
   (is (= (sudoku/choice 1) [1]))))

(deftest cp-test
  (and
   (is (= (sudoku/cp [[1] [2] [3]]) [[1 2 3]]))
   (is (= (sudoku/cp [[1 2] [] [4 5]]) []))))
