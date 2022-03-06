(ns ^:figwheel-hooks sudoku-clojure.core
  (:require
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom]]
   [reagent.dom :as rdom]))

(println "This text is printed from src/sudoku_clojure/core.cljs. Go ahead and edit it and see reloading in action.")

(defn multiply [a b] (* a b))

;; define your app data so that it doesn't get over-written on reload
(defonce app-state (atom {:text "sudoku"}))

(defn get-app-element []
  (gdom/getElement "app"))

(defn main []
  [:div
   [:p (:text @app-state)]])

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
      (for [x (first matrix) ys (cp (rest matrix))]
        (cons x ys))))


;; expand :: Matrix [Digit] -> [Grid]
;; expand = cp . map cp
;; compose haskell-style; last one is partial for point-free
;; (def expand (comp cp (partial map cp)))
(def expand #(->> % (map cp) cp))

;; alternates
;; (defn expand_2 [digit-matrix] (->> digit-matrix (map cp) cp))
;; (defn expand_3 [dm] (map #(map cp %) dm))

;; rows :: Matrix a -> Matrix a
;; rows = id
(def rows #(identity %))
;; (def rows (partial identity))
;; (defn rows [x] (identity x))

;; cols :: Matrix a -> Matrix a
;; cols [xs] = [[x] | x <- xs]
;; cols (xs:xss) = zipWith (:) xs (cols xss)
(defn cols [m] (apply map vector m))

;; group :: [a] -> [[a]]
;; group [] = []
;; -- group xs = take 3 xs:group (drop 3 xs)
;; group xs = take (fromInteger boxsize) xs:group (drop (fromInteger boxsize) xs)
(defn group [xs] (if (nil? xs) [] (partition boxsize xs)))
(defn group2 [xs] (map #(let [size (/ (count %) 2)] partition size %) xs))
;; (defn group3 [xs] (partition 2 %)

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

;; TODO do valid, then check naive solve with small boards
;; valid :: Grid -> Bool
;; valid g = all nodups (rows g) &&
;; all nodups (cols g) &&
;; all nodups (boxs g)
(defn valid [g]
  (and (every? nodups (rows g))
       (every? nodups (rows g))
       (every? nodups (rows g))))


;; solve :: Grid -> [Grid]
;; solve grid = filter valid . expand . choices grid
(defn solve [grid]
  (->> grid
       choices
       expand
       (filter valid)))





(defn mount [el]
  (rdom/render [main] el))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))

;; conditionally start your application based on the presence of an "app" element
;; this is particularly helpful for testing this ns without launching the app
(mount-app-element)

;; specify reload hook with ^:after-load metadata
(defn ^:after-load on-reload []
  (mount-app-element)
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )
