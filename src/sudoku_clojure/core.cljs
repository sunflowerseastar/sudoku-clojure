(ns ^:figwheel-hooks sudoku-clojure.core
  (:require
   [clojure.string :refer [replace-first]]
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom create-class]]
   [reagent.dom :as rdom]
   [sudoku-clojure.boards :refer [boards]]
   [sudoku-clojure.sudoku :refer [solve]]))

(defn get-app-element []
  (gdom/getElement "app"))

(def current-board-index (atom 0))
(def board (atom (nth boards @current-board-index)))

(def solutions (atom '()))
(def current-solution-index (atom 0))

;; ui
(def is-board-pristine (atom true))
(def is-solving (atom false))
(def is-success (atom false))
(def has-initially-loaded (atom false))

(defn clear-ui! []
  (do (reset! is-board-pristine true)
      (reset! is-solving false)
      (reset! solutions '())
      (reset! current-solution-index 0)
      (reset! is-success false)))

(defn previous-or-next-board! [dec-or-inc]
  (let [new-board-index (mod (dec-or-inc @current-board-index) (count boards))]
    (do (clear-ui!)
        (reset! current-board-index new-board-index)
        (reset! board (nth boards new-board-index)))))

(defn previous-or-next-solution! [dec-or-inc]
  (let [new-solution-index (mod (dec-or-inc @current-solution-index) (count @solutions))]
    (do (reset! current-solution-index new-solution-index)
        (reset! board (nth @solutions new-solution-index)))))

(defn solve! []
  (do (reset! is-solving true)
      (let [new-solutions (solve @board)]
        (do (reset! is-solving false)
            (reset! is-board-pristine true)
            (reset! is-success true)
            (reset! solutions new-solutions)
            (when-not (empty? new-solutions) (reset! board (first new-solutions)))))))

(defn update-board-x-y! [x y new-value]
  (do
    (clear-ui!)
    (reset! is-board-pristine false)
    (swap! board assoc-in [y x] new-value)))

(defn square-c [x y square update-board-fn]
  [:div.square
   {:style {:grid-column (+ x 1) :grid-row (+ y 1)}}
   [:input {:type "text" :value (when (not (zero? square)) square)
            :on-change #(let [;; remove old value (square) from (->> % .-target .-value)
                              new-value (replace-first (->> % .-target .-value) (re-pattern (str square)) "")]
                          (if (re-matches #"[1-9]" new-value)
                            (update-board-fn x y (js/parseInt new-value))
                            (update-board-fn x y 0)))}]])

(defn main []
  (create-class
   {:component-did-mount
    (fn [] (js/setTimeout #(reset! has-initially-loaded true) 0))
    :reagent-render
    (fn [this]
      [:div.main
       {:class (if @has-initially-loaded "has-initially-loaded")}
       [:div.board-container
        [:div.above-board.constrain-width
         [:div.left
          [:a.arrow-left {:on-click #(previous-or-next-board! dec)} "◀"]
          [:a.arrow-right {:on-click #(previous-or-next-board! inc)} "▶"]
          [:span.em {:class (when (not @is-board-pristine) "is-dimmed")}
           (str "board " (inc @current-board-index) " of " (count boards))]]]
        [:div.board.constrain-width
         [:div.board-inner
          (let [x-shape (count (first @board))
                y-shape (count @board)]
            (map-indexed
             (fn [y row]
               (map-indexed
                (fn [x square]
                  ^{:key (str x y)}
                  [square-c x y square update-board-x-y!])
                row))
             @board))
          [:div.board-horizontal-lines " "]
          [:div.board-vertical-lines " "]]]
        [:div.below-board.constrain-width
         [:div.left {:class (when (not @is-success) "is-hidden")}
          (cond (empty? @solutions) [:span.em "no solutions found"]
                (= (count @solutions) 1) [:span.em "1 solution found"]
                (> (count @solutions) 1)
                [:<>
                 [:a.arrow-left {:on-click #(previous-or-next-solution! dec)} "◀"]
                 [:a.arrow-right {:on-click #(previous-or-next-solution! inc)} "▶"]
                 [:span.em (str "solution " (inc @current-solution-index) " of " (count @solutions))]])]]]
       [:div.button-container
        [:div.button-indicator
         {:class [(when @is-success "is-success")
                  (when @is-solving "is-solving")]}
         [:button {:on-click #(when (and (false? @is-success)
                                         (false? @is-solving))
                                (solve!))}
          "solve"]]]])}))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (rdom/render [main] el)))
(mount-app-element)

(defn ^:after-load on-reload [] ;; reload hook
  (mount-app-element))
