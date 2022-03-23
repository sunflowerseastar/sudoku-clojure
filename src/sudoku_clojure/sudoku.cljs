(ns sudoku-clojure.sudoku)

(def boxsize 3)
(def digits (range 1 10))

(defn choice [d] (if (zero? d) digits [d]))

;; choices :: Grid -> Matrix [Digit]
(def choices (partial map (partial map choice)))

;; cp :: [[a]] -> [[a]]
(defn cp [matrix]
  (if (empty? matrix) [[]]
      (let [yss (cp (rest matrix))]
        (for [x (first matrix) ys yss]
          (cons x ys)))))


;; expand :: Matrix [Digit] -> [Grid]
(defn expand [matrix] (->> matrix (map cp) cp))
;; (def expand_1 #(->> % (map cp) cp))
;; (def expand_2 (comp cp (partial map cp)))
;; (defn expand_3 [dm] (map #(map cp %) dm))

;; rows :: Matrix a -> Matrix a
(defn rows [x] (identity x))
;; (def rows (partial identity))

;; cols :: Matrix a -> Matrix a
(defn cols [m] (apply map vector m))

;; group :: [a] -> [[a]]
(defn group [xs] (partition boxsize xs))

;; ungroup :: [[a]] -> [a]
(def ungroup (partial apply concat))

;; boxs :: Matrix a -> Matrix a
(defn boxs [m] (->> m (map group) group (map cols) ungroup (map ungroup)))

;; nodups :: (Eq a) => [a] -> Bool
;; nodups [] = True
;; nodups (x:xs) = all (/=x) xs && nodups xs
;; TODO write as a fold
(defn nodups [xs]
  (if (empty? xs) true
      (and (not-any? #(== (first xs) %) (rest xs))
           (nodups (rest xs)))))

;; valid :: Grid -> Bool
(defn valid [g]
  (and (every? nodups (rows g))
       (every? nodups (rows g))
       (every? nodups (rows g))))

;; remove :: [Digit] -> [Digit] -> [Digit]
;; list "subtraction"
(defn remove-fixed [ds xs]
  (if (= (count xs) 1) xs
      (remove #(some #{%} ds) xs)))

(def r [[2] [3] [9] [1 2 3 4 5 6 7 8 9] [4] [1] [5] [6] [1 2 3 4 5 6 7 8 9]])

;; -- remove the row's singletons ("fixed" entries) from the row's lists
(defn prune-row [row]
  (let [fixed (for [[d :as ds] row :when (= (count ds) 1)] d)]
    (map #(remove-fixed fixed %) row)))

(defn prune-by [f matrix]
  (->> matrix f (map prune-row) f))

;; prune :: Matrix [Digit] -> Matrix [Digit]
(defn prune [matrix]
  (->> matrix (prune-by rows) (prune-by cols) (prune-by boxs)))

;; many :: (Eq a) => (a -> a) -> a -> a
(defn many [f x] (let [y (f x)] (if (= x y) x (recur f y))))


;; solve :: Grid -> [Grid]
;; solve grid = filter valid . expand . choices grid
;; (defn solve [grid] (->> grid choices (many prune) expand (filter valid)))


(defn span [p xs]
  (loop [[x & tail :as xs] xs acc []]
    (cond (nil? xs) [acc []]
          (and (not (empty? xs)) (p x)) (recur tail (conj acc x))
          :else [acc xs])))

;; -- prelude
;; -- break :: (a -> Bool) -> [a] -> ([a],[a])
;; TODO swap out with split-with https://clojuredocs.org/clojure.core/split-with
(defn break [p xs] (span (comp not p) xs))

(defn counts [xs] (->> xs
                       (map (partial map count))
                       flatten
                       (filter #(not= 1 %))))

;; expand1 :: Matrix [Digit] -> [Matrix [Digit]]
(defn expand1 [rows]
  (let [n (apply min (counts rows))
        [rows1 [row & rows2]] (break (fn [xs] (some #(= (count %) n) xs)) rows)
        [row1 [cs & row2]] (break #(= (count %) n) row)]
    (for [c cs] (concat rows1 [(concat row1 (cons [c] row2))] rows2))))

;; single :: [a] -> Bool
(defn single [x] (= (count x) 1))

;; complete :: Matrix [Digit] -> Bool
(defn complete [matrix] (every? #(every? single %) matrix))

(defn ok [row] (nodups (for [[x :as xs] row :when (= (count xs) 1)] x)))

;; safe :: Matrix [Digit] -> Bool
(defn safe [cm] (and (every? ok (rows cm))
                     (every? ok (cols cm))
                     (every? ok (boxs cm))))

;; extract :: Matrix [Digit] -> Grid
(defn extract [matrix] (map #(map first %) matrix))

(defn search [cm]
  (let [pm (prune cm)]
    (cond (not (safe pm)) []
          (complete pm) [(extract pm)]
          :else (apply concat (map search (expand1 pm))))))

;; solve :: Grid -> [Grid]
(defn solve [grid] (-> grid choices search))
