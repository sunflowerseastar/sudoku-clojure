(ns sudoku-clojure.theme
  "Theme cycles through :system, :dark, and :light."
  (:require
   [reagent.core :refer [atom]]))

(def ^:private storage-key "sudoku-clojure.theme")
(def ^:private query-key "theme")

;; cycle order: :system -> :dark -> :light -> :system ...
(def preferences [:system :dark :light])

(def ^:private next-preference (zipmap preferences (rest (cycle preferences))))

(defn- preference-from-string
  "Parse a stored/query string into a preference."
  [s]
  (some #(when (= (name %) s) %) preferences))

(def preference (atom :system))

(def query-override (atom nil))

(def system-dark? (atom false))

(defn effective-preference
  []
  (or @query-override @preference))

(defn resolved-theme
  []
  (let [in-force (effective-preference)]
    (if (= in-force :system)
      (if @system-dark? :dark :light)
      in-force)))

(defn- apply-to-document! []
  (.setAttribute (.-documentElement js/document) "data-theme" (name (resolved-theme))))

(defn- read-stored []
  (try
    (preference-from-string (.getItem (.-localStorage js/window) storage-key))
    (catch :default _ nil)))

(defn- store! [new-preference]
  (try
    (.setItem (.-localStorage js/window) storage-key (name new-preference))
    (catch :default _ nil)))

(defn- read-query []
  (-> (js/URL. (.. js/window -location -href))
      .-searchParams
      (.get query-key)
      preference-from-string))

(defn- drop-query-param!
  []
  (let [url (js/URL. (.. js/window -location -href))]
    (when (.has (.-searchParams url) query-key)
      (.delete (.-searchParams url) query-key)
      (.replaceState (.-history js/window) nil "" (str url)))))

(defn cycle-theme!
  []
  (let [new-preference (next-preference (effective-preference))]
    (reset! preference new-preference)
    (reset! query-override nil)
    (store! new-preference)
    (drop-query-param!)
    (apply-to-document!)))

(defn init! []
  (reset! preference (or (read-stored) :system))
  (reset! query-override (read-query))
  (let [media-query (.matchMedia js/window "(prefers-color-scheme: dark)")]
    (reset! system-dark? (.-matches media-query))
    (.addEventListener media-query "change"
                       #(do (reset! system-dark? (.-matches %))
                            (apply-to-document!))))
  (apply-to-document!))

(defn- theme-icon-c
  [in-force]
  [:svg.theme-icon
   {:viewBox "0 0 16 16" :aria-hidden true}
   [:circle {:cx 8 :cy 8 :r 6.25
             :fill (if (= in-force :dark) "currentColor" "none")
             :stroke "currentColor" :stroke-width 1.5}]
   (when (= in-force :system)
     [:path {:d "M8 1.75 A6.25 6.25 0 0 0 8 14.25 Z" :fill "currentColor"}])])

(defn theme-toggle-c []
  (let [in-force (effective-preference)
        label (str "theme: " (name in-force) " (click to cycle)")]
    [:a.theme-toggle
     {:on-click #(cycle-theme!) :title label :aria-label label}
     [theme-icon-c in-force]]))
