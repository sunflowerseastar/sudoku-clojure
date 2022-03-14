(ns sudoku-clojure.sudoku)

(def boxsize 3)

(def b1 [[0 9 0 0 0 0 0 4 0]
         [0 6 4 2 0 5 7 8 0]
         [0 0 8 0 4 0 9 0 0]
         [4 8 3 0 6 0 2 5 1]
         [9 0 0 0 0 0 0 0 7]
         [2 5 7 0 8 0 4 6 9]
         [0 0 9 0 1 0 5 0 0]
         [0 3 1 8 0 2 6 9 0]
         [0 4 0 0 0 0 0 1 0]])
(def b2 [[1 2 4 3] [3 4 2 1] [2 1 3 4] [4 0 1 2]])
;; https://www.7sudoku.com/images/examples/solved_puzzle.png
(def b3 [[8 2 7 1 5 4 3 9 6]
         [9 6 5 3 2 7 1 4 8]
         [3 4 1 6 8 9 7 5 2]
         [5 9 3 4 6 8 2 7 1]
         [4 7 2 5 1 3 6 8 9]
         [6 1 8 9 7 2 4 3 5]
         [7 8 6 2 3 5 9 1 4]
         [1 5 4 7 9 6 8 2 3]
         [2 3 9 8 4 1 5 6 0]]) ;; 0 => 7
(def b3b [[8 2 7 1 5 4 3 9 6]
          [9 0 0 3 0 7 0 0 8]
          [0 0 0 6 0 9 0 5 2]
          [0 9 3 4 6 8 2 0 1]
          [0 0 0 0 0 0 0 0 9]
          [6 0 8 9 7 0 4 3 5]
          [7 0 0 2 0 0 0 1 4]
          [1 5 4 7 0 6 8 2 3]
          [2 3 9 8 4 1 5 6 0]])
(def b4 [[1 2] [3 4]])
(def b5 [[1 2 3]  [4 5 6]  [7 8 9]])


;; cp :: [[a]] -> [[a]]

(def digits (range 1 10))



(defn choice [d] (if (zero? d) digits [d]))

;; choices :: Grid -> Matrix [Digit]
(def choices (partial map (partial map choice)))
;; (choices b2)

;; cp :: [[a]] -> [[a]]
(defn cp [matrix]
  (if (empty? matrix) [[]]
      (let [yss (cp (rest matrix))]
        (for [x (first matrix) ys yss]
          (cons x ys)))))


;; expand :: Matrix [Digit] -> [Grid]
;; expand = cp . map cp
(def expand #(->> % (map cp) cp))

;; alternates
;; compose haskell-style; last one is partial for point-free
;; (def expand_2 (comp cp (partial map cp)))
;; (defn expand_3 [digit-matrix] (->> digit-matrix (map cp) cp))
;; (defn expand_4 [dm] (map #(map cp %) dm))

;; rows :: Matrix a -> Matrix a
;; rows = id
(defn rows [x] (identity x))
;; (def rows (partial identity))

;; cols :: Matrix a -> Matrix a
;; cols [xs] = [[x] | x <- xs]
;; cols (xs:xss) = zipWith (:) xs (cols xss)
(defn cols [m] (apply map vector m))

;; group :: [a] -> [[a]]
;; group [] = []
;; -- group xs = take 3 xs:group (drop 3 xs)
;; group xs = take (fromInteger boxsize) xs:group (drop (fromInteger boxsize) xs)
(defn group [xs] (partition boxsize xs))
;; TODO confirm that (if (nil? xs) [] ...) isn't needed
;; (defn group2..? [xs] (if (nil? xs) [] (partition boxsize xs)))
;; (defn group3 [xs] (map #(let [size (/ (count %) 2)] partition size %) xs))

;; ungroup :: [[a]] -> [a]
;; ungroup = concat
(def ungroup (partial apply concat))

;; boxs :: Matrix a -> Matrix a
;; boxs = map ungroup . ungroup . map cols . group . map group
(defn boxs [m] (->> m
                    (map group)
                    group
                    (map cols)
                    ungroup
                    (map ungroup)))

;; nodups :: (Eq a) => [a] -> Bool
;; nodups [] = True
;; nodups (x:xs) = all (/=x) xs && nodups xs
(defn nodups [xs]
  (if (empty? xs) true
      (and (not-any? #(== (first xs) %) (rest xs))
           (nodups (rest xs)))))

;; valid :: Grid -> Bool
;; valid g = all nodups (rows g) && all nodups (cols g) && all nodups (boxs g)
(defn valid [g]
  (and (every? nodups (rows g))
       (every? nodups (rows g))
       (every? nodups (rows g))))



;; -- remove [0,3] [0] => [0]
;; -- remove [0,3] [1,2] => [1,2]
;; -- remove [0,3] [1,3,4] => [1,4]
;; remove :: [Digit] -> [Digit] -> [Digit]
;; remove ds [x] = [x]
;; remove ds xs = filter (`notElem` ds) xs
;; (remove-fixed [0 3] [0]) => [0]
;; (remove-fixed [0 3] [1 2]) => [1 2]
;; (remove-fixed [0 3] [1 3 4]) => [1 4]
;; list "subtraction" (or subtraction), sorta
(defn remove-fixed [ds xs]
  (if (= (count xs) 1) xs
      (remove #(some #{%} ds) xs)))

;; (prune-row [[0] [1 2] [3] [1 3 4] [5 6]]) => [[0] [1 2] [3] [1 4] [5 6]]
;; (prune-row [[6] [3 6] [3] [1 3 4] [4]]) => [[6] [] [3] [1] [4]]
;; (prune-row [[2] [3] [9] [1 2 3 4 5 6 7 8 9] [4] [1] [5] [6] [1 2 3 4 5 6 7 8 9]])
(def r [[2] [3] [9] [1 2 3 4 5 6 7 8 9] [4] [1] [5] [6] [1 2 3 4 5 6 7 8 9]])

;; -- remove the row's singletons ("fixed" entries) from the row's lists
;; pruneRow :: Row [Digit] -> Row [Digit]
;; pruneRow row = map (remove fixed) row
;;   where fixed = [d | [d] <- row]
(defn prune-row [row]
  (let [fixed (for [[d :as ds] row :when (= (count ds) 1)] d)]
    (map #(remove-fixed fixed %) row)))


;; pruneBy f = f . map pruneRow . f
(defn prune-by [f matrix]
  (->> matrix f (map prune-row) f))

;; prune :: Matrix [Digit] -> Matrix [Digit]
;; prune = pruneBy boxs . pruneBy cols . pruneBy rows
(defn prune [matrix]
  (->> matrix (prune-by rows) (prune-by cols) (prune-by boxs)))

;; many :: (Eq a) => (a -> a) -> a -> a
;; many f x = if x == y then x else many f y
;; where y = f x
(defn many [f x] (let [y (f x)] (if (= x y) x (recur f y))))


;; solve :: Grid -> [Grid]
;; solve grid = filter valid . expand . choices grid
;; (defn solve [grid] (->> grid choices (many prune) expand (filter valid)))

;; TODO wire into interface
;; TODO add single | expand1 | counts | complete | safe | ok | extract | solve | search

;; -- prelude
;; -- break :: (a -> Bool) -> [a] -> ([a],[a])
;; -- break p = span (not . p)
;; (break even? [1 3 7 6 2 3 5]) => ([1 3 7] [6 2 3 5])
;; >>> span (< 3) [1,2,3,4,1,2,3,4] => ([1,2],[3,4,1,2,3,4])
;; >>> span (< 9) [1,2,3] => ([1,2,3],[])
;; >>> span (< 0) [1,2,3] => ([],[1,2,3])
(defn span [p xs]
  (loop [[x & tail :as xs] xs acc []]
    (if (p x) (recur tail (conj acc x))
        [acc xs])))

(defn break [p xs] (span (comp not p) xs))

;; counts = filter (/= 1) . map length . concat
(defn counts [xs] (->> xs
                       (map (partial map count))
                       flatten
                       (filter #(not= 1 %))
                       ;; (apply min)
                       ))

;; expand1 :: Matrix [Digit] -> [Matrix [Digit]]
;; expand1 rows
;; = [rows1 ++ [row1 ++ [c]:row2] ++ rows2 | c <- cs]
;;   where
;;   (rows1, row:rows2) = break (any smallest) rows
;;   (row1, cs:row2)    = break smallest row
;;   smallest cs        = length cs == n
;;   n                  = minimum (counts rows)
(defn expand1 [rows]
  (let [n (apply min (counts rows))
        [rows1 [row & rows2]] (break (fn [xs] (some #(= (count %) n) xs)) rows)
        [row1 [cs & row2]] (break #(= (count %) n) row)]
    (for [c cs] (concat rows1 [(concat row1 (cons [c] row2))] rows2))))

(def x (->> b1 choices prune))

;; single :: [a] -> Bool
;; single [_] = True
;; single _ = False
(defn single [x] (= (count x) 1))

;; complete :: Matrix [Digit] -> Bool
;; complete = all (all single)
(defn complete [matrix] (every? #(every? single %) matrix))

;; ok row = nodups [x | [x] <- row]
(defn ok [row] (nodups (for [[x :as xs] row :when (= (count xs) 1)] x)))

;; safe :: Matrix [Digit] -> Bool
;; safe cm = all ok (rows cm) &&
;;   all ok (cols cm) &&
;;   all ok (boxs cm)
(defn safe [cm] (and (every? ok (rows cm))
                     (every? ok (cols cm))
                     (every? ok (boxs cm))))

;; extract :: Matrix [Digit] -> Grid
;; extract = map (map head)
(defn extract [matrix] (map #(map first %) matrix))

;; search cm
;;   | not (safe pm) = []
;;   | complete pm = [extract pm]
;;   | otherwise = concat (map search (expand1 pm))
;;     where pm = prune cm
(defn search [cm]
  (let [pm (prune cm)]
    (cond (not (safe pm)) []
          (complete pm) [(extract pm)]
          :else (apply concat (map search (expand1 pm))))))

;; solve :: Grid -> [Grid]
;; solve = search . choices
(defn solve [grid] (-> grid choices search))
