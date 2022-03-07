(ns ^:figwheel-hooks sudoku-clojure.core
  (:require
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom]]
   [reagent.dom :as rdom]
   [sudoku-clojure.sudoku :refer [solve]]))

(def b1 [[0 9 0 0 0 0 0 4 0]
         [0 6 4 2 0 5 7 8 0]
         [0 0 8 0 4 0 9 0 0]
         [4 8 3 0 6 0 2 5 1]
         [9 0 0 0 0 0 0 0 7]
         [2 5 7 0 8 0 4 6 9]
         [0 0 9 0 1 0 5 0 0]
         [0 3 1 8 0 2 6 9 0]
         [0 4 0 0 0 0 0 1 0]])

(println "hi" (solve b1))

(defn multiply [a b] (* a b))

;; define your app data so that it doesn't get over-written on reload
(defonce app-state (atom {:text "sudoku"}))

(defn get-app-element []
  (gdom/getElement "app"))

(defn main []
  [:div
   [:p (:text @app-state)]])


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
