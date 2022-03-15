(ns ^:figwheel-hooks sudoku-clojure.core
  (:require
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom create-class]]
   [reagent.dom :as rdom]
   [sudoku-clojure.boards :refer [boards]]
   [sudoku-clojure.sudoku :refer [solve]]))

(println "hi" boards)

(defn multiply [a b] (+ a b))

(defn get-app-element []
  (gdom/getElement "app"))

;; TODO set up a basic sudoku board in the DOM
;; TODO connect a basic board data structure with the DOM board
;; TODO think through next steps

(def current-board-index (atom 0))
(def is-board-modified (atom false))
(def board (atom (nth boards @current-board-index)))

(def solutions (atom '()))
(def current-solution-index (atom 0))

;; ui
(def is-requesting (atom false))
(def is-no-solution (atom false))
(def is-success (atom false))
(def has-initially-loaded (atom true))

(defn clear-ui! []
  (do
    ;; (reset! is-requesting false)
    ;; (reset! is-timeout false)
    ;; (reset! is-no-solution false)
    ;; (reset! is-success false)
    ;; (reset! solutions '())
    ;; (reset! current-solution-index 0)
    ))

(defn reset-board! [new-board]
  (reset! board new-board))

(defn clear-board! []
  (let [clear-values (fn [squares] (->> squares (mapv #(assoc % :value nil))))]
    (reset-board! (->> @board
                       (map clear-values)
                       vec))))

(defn clear! []
  (do (reset! is-board-modified true)
      (clear-ui!)
      (clear-board!)))

(defn previous-or-next-board! [dec-or-inc]
  (println "previous-or-next-board!"))

(defn previous-or-next-solution! [dec-or-inc]
  (println "previous-or-next-solution!"))

(defn square-c [x y square]
  [:div.square
   {:style {:grid-column (+ x 1) :grid-row (+ y 1)}}
   [:p (when (not (zero? square)) square)]])

(defn main []
  (create-class
   {:reagent-render
    (fn [this]
      [:div.main
       {:class (if @has-initially-loaded "has-initially-loaded")}
       [:div.board-container
        [:div.above-board.constrain-width
         [:div.left
          [:a.arrow-left {:on-click #(previous-or-next-board! dec)} "◀"]
          [:a.arrow-right {:on-click #(previous-or-next-board! inc)} "▶"]
          [:span.em {:class (when @is-board-modified "is-dimmed")}
           (str "board " (inc @current-board-index) " of " (count boards))]]]
        [:div.board.constrain-width
         {:style {:grid-template-rows (str "repeat(14, " (/ 100 (count @board)) "%)")}}
         (let [x-shape (count (first @board))
               y-shape (count @board)]
           (map-indexed
            (fn [y row]
              (map-indexed
               (fn [x square]
                 ^{:key (str x y)}
                 [square-c x y square])
               row))
            @board))]
        [:div.below-board.constrain-width
         [:div.left
          (cond (true? @is-no-solution) [:span.em "no solutions found"]
                (= (count @solutions) 1) [:span.em "1 solution found"]
                (> (count @solutions) 1)
                [:<>
                 [:a.arrow-left {:on-click #(previous-or-next-solution! dec)} "◀"]
                 [:a.arrow-right {:on-click #(previous-or-next-solution! inc)} "▶"]
                 [:span.em (str "solution " (inc @current-solution-index) " of " (count @solutions))]])]]]
       [:div.button-container
        [:div.button-indicator
         {:class [(when @is-success "is-success")
                  (when @is-no-solution "is-no-solution")
                  (when @is-requesting "is-requesting")]}
         [:button {:on-click #(when (and (false? @is-success)
                                         (false? @is-no-solution)
                                         (false? @is-requesting))
                                (println "stub: request solution"))}
          "solve"]]]])}))


(defn mount-app-element []
  (when-let [el (get-app-element)]
    (rdom/render [main] el)))
(mount-app-element)

(defn ^:after-load on-reload [] ;; reload hook
  (mount-app-element))
