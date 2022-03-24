(ns sudoku-clojure.sudoku)

(def boxsize 3)
(def digits (range 1 10))

(defn choice [d] (if (zero? d) digits [d]))

(def choices (partial map (partial map choice)))

(defn cp [matrix]
  (if (empty? matrix) [[]]
      (let [yss (cp (rest matrix))]
        (for [x (first matrix) ys yss]
          (cons x ys)))))

(defn expand [matrix] (->> matrix (map cp) cp))
;; (def expand_1 #(->> % (map cp) cp))
;; (def expand_2 (comp cp (partial map cp)))
;; (defn expand_3 [dm] (map #(map cp %) dm))

(defn rows [x] (identity x))
;; (def rows_1 (partial identity))

(defn cols [m] (apply map vector m))

(defn group [xs] (partition boxsize xs))

(def ungroup (partial apply concat))

(defn boxs [m] (->> m (map group) group (map cols) ungroup (map ungroup)))

;; TODO write as a fold
(defn nodups [xs]
  (if (empty? xs) true
      (and (not-any? #(== (first xs) %) (rest xs))
           (nodups (rest xs)))))

(defn valid [g]
  (and (every? nodups (rows g))
       (every? nodups (rows g))
       (every? nodups (rows g))))

(defn remove-fixed
;; list "subtraction"
  [ds xs]
  (if (= (count xs) 1) xs
      (remove #(some #{%} ds) xs)))

(def r [[2] [3] [9] [1 2 3 4 5 6 7 8 9] [4] [1] [5] [6] [1 2 3 4 5 6 7 8 9]])

(defn prune-row
;; -- remove the row's singletons ("fixed" entries) from the row's lists
  [row]
  (let [fixed (for [[d :as ds] row :when (= (count ds) 1)] d)]
    (map #(remove-fixed fixed %) row)))

(defn prune-by [f matrix]
  (->> matrix f (map prune-row) f))

(defn prune [matrix]
  (->> matrix (prune-by rows) (prune-by cols) (prune-by boxs)))

(defn many [f x] (let [y (f x)] (if (= x y) x (recur f y))))


;; solve :: Grid -> [Grid]
;; solve grid = filter valid . expand . choices grid
;; (defn solve [grid] (->> grid choices (many prune) expand (filter valid)))


(defn span [p xs]
  (loop [[x & tail :as xs] xs acc []]
    (cond (nil? xs) [acc []]
          (and (not (empty? xs)) (p x)) (recur tail (conj acc x))
          :else [acc xs])))

(defn break
  ;; -- prelude
  ;; -- break :: (a -> Bool) -> [a] -> ([a],[a])
  ;; TODO swap out with split-with https://clojuredocs.org/clojure.core/split-with
  [p xs] (span (comp not p) xs))

(defn counts [xs] (->> xs
                       (map (partial map count))
                       flatten
                       (filter #(not= 1 %))))

(defn expand1 [rows]
  (let [n (apply min (counts rows))
        [rows1 [row & rows2]] (break (fn [xs] (some #(= (count %) n) xs)) rows)
        [row1 [cs & row2]] (break #(= (count %) n) row)]
    (for [c cs] (concat rows1 [(concat row1 (cons [c] row2))] rows2))))

(defn single [x] (= (count x) 1))

(defn complete [matrix] (every? #(every? single %) matrix))

(defn ok [row] (nodups (for [[x :as xs] row :when (= (count xs) 1)] x)))

(defn safe [cm] (and (every? ok (rows cm))
                     (every? ok (cols cm))
                     (every? ok (boxs cm))))

(defn extract [matrix] (mapv #(mapv first %) matrix))

(defn search [cm]
  (let [pm (prune cm)]
    (cond (not (safe pm)) []
          (complete pm) [(extract pm)]
          :else (apply concat (map search (expand1 pm))))))

(defn solve [grid] (-> grid choices search))

