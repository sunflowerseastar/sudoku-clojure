(ns sudoku-clojure.core-test
  (:require
   [cljs.test :refer-macros [deftest is testing]]
   [sudoku-clojure.boards :as boards]
   [sudoku-clojure.sudoku :as sudoku]))

(deftest choice-test
  (is (= (sudoku/choice 0) [1 2 3 4 5 6 7 8 9]))
  (is (= (sudoku/choice 1) [1])))

(deftest cp-test
  (is (= (sudoku/cp [[1] [2] [3]]) [[1 2 3]]))
  (is (= (sudoku/cp [[1 2] [] [4 5]]) [])))

(deftest choices-test
  (is (= (sudoku/choices boards/b3) '(([8] [2] [7] [1] [5] [4] [3] [9] [6]) ([9] [6] [5] [3] [2] [7] [1] [4] [8]) ([3] [4] [1] [6] [8] [9] [7] [5] [2]) ([5] [9] [3] [4] [6] [8] [2] [7] [1]) ([4] [7] [2] [5] [1] [3] [6] [8] [9]) ([6] [1] [8] [9] [7] [2] [4] [3] [5]) ([7] [8] [6] [2] [3] [5] [9] [1] [4]) ([1] [5] [4] [7] [9] [6] [8] [2] [3]) ([2] [3] [9] [8] [4] [1] [5] [6] (1 2 3 4 5 6 7 8 9))))))

(deftest expand-test
  (is (= (->> boards/b3 sudoku/choices sudoku/expand count) 9)))

(deftest group-test
  (is (= (map sudoku/group boards/b5) '(((1 2 3)) ((4 5 6)) ((7 8 9))))))

(deftest cols-test
  (is (= (sudoku/cols boards/b5) '([1 4 7] [2 5 8] [3 6 9]))))

(deftest boxs-test
  (is (= (sudoku/boxs boards/b3) '((8 2 7 9 6 5 3 4 1) (1 5 4 3 2 7 6 8 9) (3 9 6 1 4 8 7 5 2) (5 9 3 4 7 2 6 1 8) (4 6 8 5 1 3 9 7 2) (2 7 1 6 8 9 4 3 5) (7 8 6 1 5 4 2 3 9) (2 3 5 7 9 6 8 4 1) (9 1 4 8 2 3 5 6 0)))))

(deftest nodups-test
  (is (= (sudoku/nodups []) true))
  (is (= (sudoku/nodups [1]) true))
  (is (= (sudoku/nodups [1 2]) true))
  (is (= (sudoku/nodups [1 2 1]) false)))

(deftest valid-test
  (is (= (sudoku/valid boards/broken-b3X) false)))

(deftest remove-fixed-test
  (is (= (sudoku/remove-fixed [] []) []))
  (is (= (sudoku/remove-fixed [] [0]) [0]))
  (is (= (sudoku/remove-fixed [] [1]) [1]))
  (is (= (sudoku/remove-fixed [1] []) []))
  (is (= (sudoku/remove-fixed [1 3] [0]) [0]))
  (is (= (sudoku/remove-fixed [1 3] [1]) [1]))
  (is (= (sudoku/remove-fixed [1 3] [1 3]) []))
  (is (= (sudoku/remove-fixed [0 1 3] [1 2 3]) [2]))
  (is (= (sudoku/remove-fixed [0 3] [1 3 4]) [1 4])))

(deftest prune-row-test
  (is (= (sudoku/prune-row [[0] [1 2] [3] [1 3 4] [5 6]]) [[0] [1 2] [3] [1 4] [5 6]]))
  (is (= (sudoku/prune-row [[6] [3 6] [3] [1 3 4] [4]]) [[6] [] [3] [1] [4]]))
  (is (= (sudoku/prune-row [[2] [3] [9] [1 2 3 4 5 6 7 8 9] [4] [1] [5] [6] [1 2 3 4 5 6 7 8 9]]) '([2] [3] [9] (7 8) [4] [1] [5] [6] (7 8)))))

(deftest prune-test
  (is (= (->> boards/b3b sudoku/choices sudoku/prune) '(([8] [2] [7] [1] [5] [4] [3] [9] [6]) ([9] (4 6) (5 6) [3] (2) [7] (1) (4) [8]) ((3 4) (4) (1) [6] (8) [9] (1 7) [5] [2]) ((5) [9] [3] [4] [6] [8] [2] (7) [1]) ((4) (4 7) (2) (5) (1 3) (3) (6) (8) [9]) ([6] (1) [8] [9] [7] (2) [4] [3] [5]) ([7] (6 8) (6) [2] (3) (3 5) (9) [1] [4]) ([1] [5] [4] [7] (9) [6] [8] [2] [3]) ([2] [3] [9] [8] [4] [1] [5] [6] (7))))))

(deftest many-test
  (is (= (sudoku/many #(quot % 2) 10) 0)))

(deftest span-test
  (is (= (sudoku/span #(< % 3) [1 2 3 4 1 2 3 4]) [[1 2] [3 4 1 2 3 4]]))
  (is (= (sudoku/span #(< % 9) [1 2 3]) [[1 2 3] []]))
  (is (= (sudoku/span #(< % 0) [1 2 3]) [[] [1 2 3]])))

(deftest break-test
  (is (= (sudoku/break even? [1 3 7 6 2 3 5]) '[[1 3 7] (6 2 3 5)])))

(deftest counts-test
  (is (= (sudoku/counts [[[1] [2] [2 3]] [[1 2 3] [1]]]) '(2 3)))
  (is (= (->> boards/b3b sudoku/choices sudoku/prune sudoku/rows sudoku/counts) '(2 2 2 2 2 2 2 2))))

(deftest expand1-test
  (is (= (sudoku/expand1 [[[1] [2 3]] [[1 2 3] [4]]]) '((([1] [2]) [[1 2 3] [4]]) (([1] [3]) [[1 2 3] [4]]))))
  (is (= (->> boards/b3b sudoku/choices sudoku/prune sudoku/expand1 flatten count) 176)))

(deftest single-test
  (is (= (sudoku/single [1]) true))
  (is (= (sudoku/single []) false))
  (is (= (sudoku/single [1 2]) false)))

(deftest complete-test
  (is (= (sudoku/complete [[[1] [2]] [[3] [4]]]) true))
  (is (= (sudoku/complete [[[1] [2]] [[3] [4 5]]]) false))
  (is (= (sudoku/complete [[[1] [2]] [[3] []]]) false)))

(deftest ok-test
  (is (= (sudoku/ok [[1] [2]]) true))
  (is (= (sudoku/ok [[1] [1]]) false)))

(deftest safe-test
  (is (= (sudoku/safe [[[1] [2] [3]] [[4] [5] [6]] [[7] [8] [9]]]) true))
  (is (= (sudoku/safe [[[1] [2] [3]] [[4] [5] [6]] [[7] [8] [8]]]) false))
  (is (= (sudoku/safe (->> boards/b5 sudoku/choices sudoku/prune sudoku/expand)) true)))

(deftest extract-test
  (is (= (sudoku/extract [[[1 2] [3 4]] [[5 6] [7 8]]]) '((1 3) (5 7)))))

(deftest search-test
  (is (= (->> boards/b3 sudoku/choices sudoku/search) '[((8 2 7 1 5 4 3 9 6) (9 6 5 3 2 7 1 4 8) (3 4 1 6 8 9 7 5 2) (5 9 3 4 6 8 2 7 1) (4 7 2 5 1 3 6 8 9) (6 1 8 9 7 2 4 3 5) (7 8 6 2 3 5 9 1 4) (1 5 4 7 9 6 8 2 3) (2 3 9 8 4 1 5 6 7))])))

(deftest solve-test
  (is (= (sudoku/solve boards/b1) '(((5 9 2 7 3 8 1 4 6) (1 6 4 2 9 5 7 8 3) (3 7 8 1 4 6 9 2 5) (4 8 3 9 6 7 2 5 1) (9 1 6 5 2 4 8 3 7) (2 5 7 3 8 1 4 6 9) (6 2 9 4 1 3 5 7 8) (7 3 1 8 5 2 6 9 4) (8 4 5 6 7 9 3 1 2)))))
  (is (= (sudoku/solve boards/b3) '[((8 2 7 1 5 4 3 9 6) (9 6 5 3 2 7 1 4 8) (3 4 1 6 8 9 7 5 2) (5 9 3 4 6 8 2 7 1) (4 7 2 5 1 3 6 8 9) (6 1 8 9 7 2 4 3 5) (7 8 6 2 3 5 9 1 4) (1 5 4 7 9 6 8 2 3) (2 3 9 8 4 1 5 6 7))]))
  (is (= (sudoku/solve boards/b3b) '(((8 2 7 1 5 4 3 9 6) (9 6 5 3 2 7 1 4 8) (3 4 1 6 8 9 7 5 2) (5 9 3 4 6 8 2 7 1) (4 7 2 5 1 3 6 8 9) (6 1 8 9 7 2 4 3 5) (7 8 6 2 3 5 9 1 4) (1 5 4 7 9 6 8 2 3) (2 3 9 8 4 1 5 6 7))))))
