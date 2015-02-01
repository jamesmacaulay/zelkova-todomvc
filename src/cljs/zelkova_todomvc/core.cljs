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

(enable-console-print!)

;---- MODEL ----
;
;-- The full application state of our todo app.
;type alias Model =
;    { tasks      : List Task
;    , field      : String
;    , uid        : Int
;    , visibility : String
;    }

;
;type alias Task =
;    { description : String
;    , completed   : Bool
;    , editing     : Bool
;    , id          : Int
;    }
;
;newTask : String -> Int -> Task
;newTask desc id =
;    { description = desc
;    , completed = False
;    , editing = False
;    , id = id
;    }

(defn new-task
  [desc id]
  {:description desc
   :completed? false
   :editing? false
   :id id})

;
;emptyModel : Model
;emptyModel =
;    { tasks = []
;    , visibility = "All"
;    , field = ""
;    , uid = 0
;    }

(def empty-model
  {:tasks []
   :field ""
   :uid 0
   :visibility "All"})



;---- UPDATE ----
;
;-- A description of the kinds of actions that can be performed on the model of
;-- our application. See the following post for more info on this pattern and
;-- some alternatives: http://elm-lang.org/learn/Architecture.elm
;type Action
;    = NoOp
;    | UpdateField String
;    | EditingTask Int Bool
;    | UpdateTask Int String
;    | Add
;    | Delete Int
;    | DeleteComplete
;    | Check Int Bool
;    | CheckAll Bool
;    | ChangeVisibility String
;
;-- How we update our Model on a given Action?
;update : Action -> Model -> Model
;update action model =
;    case action of
;      NoOp -> model
;

(declare updates)

(defn send-action!
  [f & args]
  (async/put! updates (apply partial f args)))

(def no-op identity)

;      Add ->
;          { model |
;              uid <- model.uid + 1,
;              field <- "",
;              tasks <-
;                  if String.isEmpty model.field
;                    then model.tasks
;                    else model.tasks ++ [newTask model.field model.uid]
;          }

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

;
;      UpdateField str ->
;          { model | field <- str }

(defn update-field
  [str model]
  (assoc model :field str))

;
;      EditingTask id isEditing ->
;          let updateTask t = if t.id == id then { t | editing <- isEditing } else t
;          in
;              { model | tasks <- List.map updateTask model.tasks }


(defn editing-task
  [id editing? model]
  (let [update-task' (fn [t]
                       (if (= id (:id t))
                         (assoc t :editing? editing?)
                         t))]
    (update model :tasks (partial mapv update-task'))))


;
;      UpdateTask id task ->
;          let updateTask t = if t.id == id then { t | description <- task } else t
;          in
;              { model | tasks <- List.map updateTask model.tasks }
;


(defn update-task
  [id description model]
  (let [update-task' (fn [t]
                       (if (= id (:id t))
                         (assoc t :description description)
                         t))]
    (update model :tasks (partial mapv update-task'))))


;      Delete id ->
;          { model | tasks <- List.filter (\t -> t.id /= id) model.tasks }

(defn delete
  [id model]
  (update model :tasks (partial filterv #(not= id (:id %)))))


;
;      DeleteComplete ->
;          { model | tasks <- List.filter (not << .completed) model.tasks }

(defn delete-complete
  [model]
  (update model :tasks (partial filterv (complement :completed?))))

;
;      Check id isCompleted ->
;          let updateTask t = if t.id == id then { t | completed <- isCompleted } else t
;          in
;              { model | tasks <- List.map updateTask model.tasks }

(defn check
  [id completed? model]
  (let [update-task' (fn [t]
                       (if (= id (:id t))
                         (assoc t :completed? completed?)
                         t))]
    (update model :tasks (partial mapv update-task'))))

;
;      CheckAll isCompleted ->
;          let updateTask t = { t | completed <- isCompleted }
;          in
;              { model | tasks <- List.map updateTask model.tasks }


(defn check-all
  [completed? model]
  (update model :tasks (partial mapv #(assoc % :completed? completed?))))

;
;      ChangeVisibility visibility ->
;          { model | visibility <- visibility }
;

(defn change-visibility
  [visibility model]
  (assoc model :visibility visibility))


;---- VIEW ----
;
;
;onEnter : Signal.Message -> Attribute
;onEnter message =
;    on "keydown"
;      (Json.customDecoder keyCode is13)
;      (always message)
;
;is13 : Int -> Result String ()
;is13 code =
;  if code == 13 then Ok () else Err "not the right key code"
;
;taskEntry : String -> Html
;taskEntry task =
;    header
;      [ id "header" ]
;      [ h1 [] [ text "todos" ]
;      , input
;          [ id "new-todo"
;          , placeholder "What needs to be done?"
;          , autofocus True
;          , value task
;          , name "newTodo"
;          , on "input" targetValue (Signal.send updates << UpdateField)
;          , onEnter (Signal.send updates Add)
;          ]
;          []
;      ]

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


;todoItem : Task -> Html
;todoItem todo =
;    let className = (if todo.completed then "completed " else "") ++
;                    (if todo.editing   then "editing"    else "")
;    in
;
;    li
;      [ class className ]
;      [ div
;          [ class "view" ]
;          [ input
;              [ class "toggle"
;              , type' "checkbox"
;              , checked todo.completed
;              , onClick (Signal.send updates (Check todo.id (not todo.completed)))
;              ]
;              []
;          , label
;              [ onDoubleClick (Signal.send updates (EditingTask todo.id True)) ]
;              [ text todo.description ]
;          , button
;              [ class "destroy"
;              , onClick (Signal.send updates (Delete todo.id))
;              ]
;              []
;          ]
;      , input
;          [ class "edit"
;          , value todo.description
;          , name "title"
;          , id ("todo-" ++ toString todo.id)
;          , on "input" targetValue (Signal.send updates << UpdateTask todo.id)
;          , onBlur (Signal.send updates (EditingTask todo.id False))
;          , onEnter (Signal.send updates (EditingTask todo.id False))
;          ]
;          []
;      ]


(defn todo-item
  [todo]
  (let [class-name (str (when (:completed? todo) "completed ")
                        (when (:editing? todo) "editing"))
        stop-editing! (fn [_] (send-action! editing-task (:id todo) false))]
    ^{:key (:id todo)}
    [:li {:class class-name}
     [:div.view
      [:input.toggle {:type      "checkbox"
                      :checked   (:completed todo)
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

;
;taskList : String -> List Task -> Html
;taskList visibility tasks =
;    let isVisible todo =
;            case visibility of
;              "Completed" -> todo.completed
;              "Active" -> not todo.completed
;              "All" -> True
;
;        allCompleted = List.all .completed tasks
;
;        cssVisibility = if List.isEmpty tasks then "hidden" else "visible"
;    in
;    section
;      [ id "main"
;      , style [ ("visibility", cssVisibility) ]
;      ]
;      [ input
;          [ id "toggle-all"
;          , type' "checkbox"
;          , name "toggle"
;          , checked allCompleted
;          , onClick (Signal.send updates (CheckAll (not allCompleted)))
;          ]
;          []
;      , label
;          [ for "toggle-all" ]
;          [ text "Mark all as complete" ]
;      , ul
;          [ id "todo-list" ]
;          (List.map todoItem (List.filter isVisible tasks))
;      ]
;

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


;visibilitySwap : String -> String -> String -> Html
;visibilitySwap uri visibility actualVisibility =
;    let className = if visibility == actualVisibility then "selected" else "" in
;    li
;      [ onClick (Signal.send updates (ChangeVisibility visibility)) ]
;      [ a [ class className, href uri ] [ text visibility ] ]

(defn visibility-swap
  [uri visibility actual-visibility]
  (let [class-name (if (= visibility actual-visibility) "selected" "")]
    [:li {:on-click (fn [_] (send-action! change-visibility visibility))}
     [:a {:class class-name
          :href uri}
      visibility]]))

;controls : String -> List Task -> Html
;controls visibility tasks =
;    let tasksCompleted = List.length (List.filter .completed tasks)
;        tasksLeft = List.length tasks - tasksCompleted
;        item_ = if tasksLeft == 1 then " item" else " items"
;    in
;    footer
;      [ id "footer"
;      , hidden (List.isEmpty tasks)
;      ]
;      [ span
;          [ id "todo-count" ]
;          [ strong [] [ text (toString tasksLeft) ]
;          , text (item_ ++ " left")
;          ]
;      , ul
;          [ id "filters" ]
;          [ visibilitySwap "#/" "All" visibility
;          , text " "
;          , visibilitySwap "#/active" "Active" visibility
;          , text " "
;          , visibilitySwap "#/completed" "Completed" visibility
;          ]
;      , button
;          [ class "clear-completed"
;          , id "clear-completed"
;          , hidden (tasksCompleted == 0)
;          , onClick (Signal.send updates DeleteComplete)
;          ]
;          [ text ("Clear completed (" ++ toString tasksCompleted ++ ")") ]
;      ]

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

;view : Model -> Html
;view model =
;    div
;      [ class "todomvc-wrapper"
;      , style [ ("visibility", "hidden") ]
;      ]
;      [ section
;          [ id "todoapp" ]
;          [ lazy taskEntry model.field
;          , lazy2 taskList model.visibility model.tasks
;          , lazy2 controls model.visibility model.tasks
;          ]
;      , infoFooter
;      ]

(defn view
  [model]
  [:div.todomvc-wrapper {:style {:visibility "hidden"}}
   [:section#todoapp
    (task-entry (:field model))
    (task-list (:visibility model) (:tasks model))
    (controls (:visibility model) (:tasks model))]])


;; -------------------------
;; Views


(defn current-page []
  [:div (view empty-model)])


;---- INPUTS ----


;
;initialModel : Model
;initialModel =
;  Maybe.withDefault emptyModel getStorage
;


(def local-storage-key "zelkova-todomvc-state")

(defn get-state
  []
  (let [data (.getItem js/localStorage local-storage-key)]
    (when-not (nil? data)
      (-> data (js/JSON.parse) (js->clj :keywordize-keys true)))))

(def initial-model (or (get-state) empty-model))

;-- updates from user input
;updates : Signal.Channel Action
;updates = Signal.channel NoOp

(def updates (async/chan))

;
;-- manage the model of our application over time
;model : Signal Model
;model = Signal.foldp update initialModel (Signal.subscribe updates)


(def model (z/foldp (fn [action state] (action state))
                    initial-model
                    (z/input no-op ::updates updates)))


;
;-- wire the entire application together
;main : Signal Html
;main = Signal.map view model

(defn store-state!
  [model]
  (let [data (-> model (clj->js) (js/JSON.stringify))]
    (.setItem js/localStorage local-storage-key data)))

(defn do-side-effects!
  [model]
  (println model)
  (store-state! model))


(def main-signal (z/map (fn [m]
                          (do-side-effects! m)
                          (view m))
                        model))


;
;port focus : Signal String
;port focus =
;    let needsFocus act =
;            case act of
;              EditingTask id bool -> bool
;              _ -> False
;
;        toSelector (EditingTask id _) = ("#todo-" ++ toString id)
;    in
;        Signal.subscribe updates
;          |> Signal.keepIf needsFocus (EditingTask 0 True)
;          |> Signal.map toSelector
;
;
;-- interactions with localStorage to save the model
;port getStorage : Maybe Model
;
;port setStorage : Signal Model
;port setStorage = model


(def dom-atom
  (let [live-graph (z/spawn main-signal)]
    (z/pipe-to-atom live-graph
                    (atom (zimpl/init live-graph)))))

(defn root-component [] @dom-atom)

;; -------------------------
;; Initialize app
(defn init! []
  (reagent/render-component [root-component] (.getElementById js/document "app")))
