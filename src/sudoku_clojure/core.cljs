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

;; solve :: Grid -> [Grid]
;; (defn solve grid = filter valid . expand . choices grid)

(def boxsize 1)

(def b2 [[1 2 4 3] [3 4 2 1] [2 1 3 4] [4 0 1 2]])
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
