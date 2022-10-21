(ns ^:figwheel-hooks sudoku-clojure.core
  (:require
   [clojure.string :refer [replace]]
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
(def is-no-solution (atom false))
(def has-initially-loaded (atom false))

(defn clear-ui! []
  (do (reset! is-board-pristine true)
      (reset! is-solving false)
      (reset! solutions '())
      (reset! current-solution-index 0)
      (reset! is-success false)
      (reset! is-no-solution false)))

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
            (println "ns :: " new-solutions)
            (reset! is-board-pristine true)
            (reset! solutions new-solutions)
            (if (empty? new-solutions)
              (reset! is-no-solution true)
              (do
                (reset! is-success true)
                (reset! board (first new-solutions))))))))

(defn update-board-x-y! [x y new-value]
  (do
    (clear-ui!)
    (reset! is-board-pristine false)
    (swap! board assoc-in [y x] new-value)))

(defn square-c
  "This is the text-input component where the square's number is shown and can be
  edited. It is painfully complicated for a few reasons:
    - The sudoku logic needs the input values to be integers, while regexp'ing and
        on-change handing needs to use strings (and browser text inputs will end up
        changing ints to strings when given the chance). So it requires care to handle
        it as a string, but keep the value at rest as an int.
    - It's special/custom input handling, in that a number press (other than 0)
        immediately updates the field as the new, single number; a
        backspace/delete/space clears the field; an alpha, special, or 0 press does nothing.
    - If the user presses many numbers all at the same time, the browser will dump
        all the numbers into the on-change function's `(->> % .-target .-value)`
        together, meaning that the 'just keep the value as a single digit' logic needs
        to handle yet another edge case."
  [x y square update-board-fn]
  [:div.square
   {:style {:grid-column (+ x 1) :grid-row (+ y 1)}}
   [:input
    {:type "text" :value (when (not (zero? square)) square)
     :on-change
     #(let [;; "value" means "the value of the text input field"
            new-value-as-entered (->> % .-target .-value)
            old-value (str square)

            ;; if ex. there was '2' and the user pressed '5', then the new-value-as-entered would be '25'.
            ;; In this case, remove the old value, '2', from '25'. new-value-without-old-value will be '5'.
            new-value-without-old-value
            (if (> (count new-value-as-entered) (count old-value))
              (replace new-value-as-entered (re-pattern old-value) "")
              new-value-as-entered)

            ;; If a chaos-monkey-esque user presses many numbers at the same time, the
            ;; new-value-as-entered can be many digits. This is because the browser will
            ;; batch the input's on-change, as opposed to running this on-change
            ;; function each time (which would keep the input always at one
            ;; digit at most). For this edge case, just keep the last digit only.
            new-value-stripped
            (if (> (count new-value-without-old-value) 1)
              (subs new-value-without-old-value (dec (count new-value-without-old-value)) (count new-value-without-old-value))
              new-value-without-old-value)

            ;; if it's a 1 through 9, use it (and use it as an int, not a string)
            new-value-validated
            (cond (re-find #"[1-9]" new-value-stripped) (js/parseInt new-value-stripped)
                  ;; if it's blank (user pressed delete/backspace), empty it --
                  ;; remember that 0 (as an int) is used in the logic as "all choices"
                  (or (= new-value-stripped "") (= new-value-stripped " ")) 0
                  ;; otherwise (user pressed alpha or other key), just leave it --
                  ;; note that this uses 'square', not 'old-value', which is a string
                  :else (js/parseInt square))]
        (update-board-fn x y new-value-validated))}]])

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
         [:div.left {:class (when (and (not @is-success) (not @is-no-solution)) "is-hidden")}
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
                  (when @is-no-solution "is-no-solution")
                  (when @is-solving "is-solving")]}
         [:button {:on-click #(solve!)}
          "solve"]]]])}))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (rdom/render [main] el)))
(mount-app-element)

(defn ^:after-load on-reload [] ;; reload hook
  (mount-app-element))
