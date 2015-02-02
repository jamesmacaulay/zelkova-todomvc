(ns zelkova-todomvc.core
  "Translation of https://github.com/evancz/elm-todomvc (as of 1be0712d3fa8bf786bde84d8db5f685e23413876)"
  (:require [reagent.core :as reagent :refer [atom]]
            [reagent.session :as session]
            [jamesmacaulay.zelkova.signal :as z]
            [jamesmacaulay.zelkova.impl.signal :as zimpl]
            [cljs.core.async :as async]
            [clojure.string :as str]
            [goog.events :as events]
            [goog.history.EventType :as EventType]))

;---- MODEL ----

(defn new-task
  [desc id]
  {:description desc
   :completed? false
   :editing? false
   :id id})

(def empty-model
  {:tasks []
   :field ""
   :uid 0
   :visibility "All"})

(def local-storage-key "zelkova-todomvc-state")

(defn get-state
  []
  (let [data (.getItem js/localStorage local-storage-key)]
    (when-not (nil? data)
      (-> data (js/JSON.parse) (js->clj :keywordize-keys true)))))

(def initial-model (or (get-state) empty-model))

;---- UPDATE ----

(declare updates)

(defn send-action!
  [f & args]
  (async/put! updates (fn [model]
                        (apply f model args))))

(defn add
  [model]
  (let [tasks (if (str/blank? (:field model))
                (:tasks model)
                (conj (:tasks model)
                      (new-task (:field model) (:uid model))))]
    (-> model
        (update :uid inc)
        (assoc :field ""
               :tasks tasks))))

(defn update-field
  [model str]
  (assoc model :field str))

(defn editing-task
  [model id editing?]
  (let [update-task' (fn [t]
                       (if (= id (:id t))
                         (assoc t :editing? editing?)
                         t))]
    (update model :tasks (partial mapv update-task'))))

(defn update-task
  [model id description]
  (let [update-task' (fn [t]
                       (if (= id (:id t))
                         (assoc t :description description)
                         t))]
    (update model :tasks (partial mapv update-task'))))

(defn delete
  [model id]
  (update model :tasks (partial filterv #(not= id (:id %)))))

(defn delete-complete
  [model]
  (update model :tasks (partial filterv (complement :completed?))))

(defn check
  [model id completed?]
  (let [update-task' (fn [t]
                       (if (= id (:id t))
                         (assoc t :completed? completed?)
                         t))]
    (update model :tasks (partial mapv update-task'))))

(defn check-all
  [model completed?]
  (update model :tasks (partial mapv #(assoc % :completed? completed?))))

(defn change-visibility
  [model visibility]
  (assoc model :visibility visibility))

;---- VIEW ----

(defn when-enter-key
  [f]
  (fn [event]
    (when (= 13 (.-keyCode event))
      (f event))))

(defn task-entry
  [task]
  [:header#header
   [:h1 "todos"]
   [:input#new-todo {:placeholder "What needs to be done?"
                     :autofocus   true
                     :value       task
                     :name        "newTodo"
                     :on-change   (fn [event]
                                    (let [text (-> event .-target .-value)]
                                      (send-action! update-field text)))
                     :on-key-down (when-enter-key (fn [_] (send-action! add)))}]])

(defn todo-item
  [todo]
  (let [class-name (str (when (:completed? todo) "completed ")
                        (when (:editing? todo) "editing"))
        stop-editing! (fn [_] (send-action! editing-task (:id todo) false))]
    ^{:key (:id todo)}
    [:li {:class class-name}
     [:div.view
      [:input.toggle {:type      "checkbox"
                      :checked   (:completed? todo)
                      :on-change (fn [_] (send-action! check (:id todo) (not (:completed? todo))))}]
      [:label {:on-double-click (fn [_] (send-action! editing-task (:id todo) true))}
       (:description todo)]
      [:button.destroy {:on-click (fn [_] (send-action! delete (:id todo)))}]]
     [:input.edit {:value       (:description todo)
                   :name        "title"
                   :id          (str "todo-" (:id todo))
                   :on-change   (fn [event]
                                  (let [text (-> event .-target .-value)]
                                    (send-action! update-task (:id todo) text)))
                   :on-blur     stop-editing!
                   :on-key-down (when-enter-key stop-editing!)}]]))

(defn task-list
  [visibility tasks]
  (let [visible? (fn [todo]
                   (case visibility
                     "Completed" (:completed? todo)
                     "Active" (not (:completed? todo))
                     "All" true))
        all-completed (every? :completed? tasks)
        css-visibility (if (empty? tasks) "hidden" "visible")]
    [:section#main {:style {:visibility css-visibility}}
     [:input#toggle-all {:type "checkbox"
                         :name "toggle"
                         :checked all-completed
                         :on-change (fn [_] (send-action! check-all (not all-completed)))}]
     [:label {:for "toggle-all"
              :text "Mark all as complete"}]
     [:ul#todo-list
      (map todo-item (filterv visible? tasks))]]))

(defn visibility-swap
  [uri visibility actual-visibility]
  (let [class-name (if (= visibility actual-visibility) "selected" "")]
    [:li {:on-click (fn [_] (send-action! change-visibility visibility))}
     [:a {:class class-name
          :href uri}
      visibility]]))

(defn controls
  [visibility tasks]
  (let [tasks-completed (count (filter :completed? tasks))
        tasks-left (- (count tasks) tasks-completed)
        item- (if (= 1 tasks-left) " item" " items")]
    [:footer#footer {:hidden (empty? tasks)}
     [:span#todo-count
      [:strong (str tasks-left)] (str item- " left")]
     [:ul#filters
      (visibility-swap "#/" "All" visibility)
      " "
      (visibility-swap "#/active" "Active" visibility)
      " "
      (visibility-swap "#/completed" "Completed" visibility)]
     [:button#clear-completed.clear-completed {:hidden (= 0 tasks-completed)
                                               :on-click (fn [_] (send-action! delete-complete))}
      (str "Clear completed (" tasks-completed ")")]]))

(defn view
  [model]
  [:div.todomvc-wrapper {:style {:visibility "hidden"}}
   [:section#todoapp
    (task-entry (:field model))
    (task-list (:visibility model) (:tasks model))
    (controls (:visibility model) (:tasks model))]])

;---- INPUTS ----

(def updates (async/chan))

(def model (z/foldp (fn [action state] (action state))
                    initial-model
                    (z/input identity ::updates updates)))

(defn store-state!
  [model]
  (let [data (-> model (clj->js) (js/JSON.stringify))]
    (.setItem js/localStorage local-storage-key data)))

(def main-signal (z/map (fn [m]
                          (println m)
                          (store-state! m)
                          (view m))
                        model))

(def dom-atom
  (let [live-graph (z/spawn main-signal)]
    (z/pipe-to-atom live-graph
                    (atom (zimpl/init live-graph)))))

(defn root-component [] @dom-atom)

;; -------------------------
;; Initialize app
(defn init! []
  (reagent/render-component [root-component] (.getElementById js/document "app")))
