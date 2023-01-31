(ns ski-tracks.core
  (:require
    [day8.re-frame.http-fx]
    [reagent.dom :as rdom]
    [reagent.core :as r]
    [re-frame.core :as rf]
    [goog.events :as events]
    [goog.history.EventType :as HistoryEventType]
    [markdown.core :refer [md->html]]
    [ski-tracks.ajax :as ajax]
    [ski-tracks.events]
    [reitit.core :as reitit]
    [reitit.frontend.easy :as rfe]
    [clojure.string :as string]
    [re-com.core :as rc :refer [at]])
  (:import goog.History))

 (defn event-val [e]
   (some-> e .-target .-value))

(defn rf-dp-fn [& args]
  (let [args (if (sequential? (first args)) (first args) args)]
  (fn [_]
    (rf/dispatch (vec args))
  )))

(defn on-click-nav [& args]
  (rf-dp-fn (vec (cons :common/navigate! args))))

(defn- button
  [& args]
  (let [[text on-click opts key] args]
  (if key
    ^{:key key}[:button (merge opts {:on-click on-click}) text]
    [:button (merge opts {:on-click on-click}) text])))

(defn- toggle-opt [toggle]
  {:style {:color "white"
    :background-color (if toggle "green" "red")}})

(defn- rc-toggle-style [toggle]
  {:color "white"
    :background-color (if toggle "green" "red")})

(defn toggle-button
  ([text on-click tog key] (button text on-click (toggle-opt tog) key))
  ([text on-click tog] (button text on-click (toggle-opt tog))))

(defn- disable-opt [toggle]
  (if toggle {:disabled "disabled"} {}))

(defn disable-button
  ([text on-click tog] (button text on-click (disable-opt tog)))
  ([text on-click tog key] (button text on-click (disable-opt tog) key)))

 (defn add-button
   [& args]
   (let [{:keys [added-opts] :or {added-opts {}}}
          (if (-> args last map?) (last args) {})
          args (if (-> args last map?) (butlast args) args)
          [text add-kind redirect add-path key] args
     on-click (on-click-nav add-kind
       {:type (string/join "/" (map name add-path))
       :key (random-uuid)} redirect)]
     (button text on-click added-opts key)))

 (defn select-buttons [item-path item-info redirect]
   [rc/h-box
   :children (conj
   (vec ( for [[opt-key opt-info] (:options item-info)]
   ^{:key (str item-path "-" opt-key)}[rc/button
    :label (:name opt-info)
    :on-click (rf-dp-fn (if (= (:type item-info) "multiselect") :toggle-multi-select :toggle-single-select) item-path opt-key)
    :style (rc-toggle-style (:selected opt-info)) ; Should use class + css stylesheet
    ]
    ))
 ^{:key (str item-path "-add")}[rc/button
   :label (str "Add " (:name item-info))
   :on-click (on-click-nav :add-item {:type (string/join "/" (map name item-path)) :key (random-uuid)} redirect)
     ])])

(defn non-button-select [item-path item-info info]
  (let [model-val (item-info :selected)
        on-change-fn (fn [e] (rf/dispatch [:add-entry-db-info item-path e]) )
        key-val (str item-path "-input")
  ]
  (case (:type item-info)
        "string" ^{:key key-val}[rc/input-text
        :model model-val
        :on-change on-change-fn
        ]
        "date" ^{:key key-val}[rc/datepicker-dropdown
        :model model-val
        :on-change on-change-fn
        ])))

 (defn select-area
   ([iterable redirect] (select-area iterable redirect []))
   ([iterable redirect path-prefix]
     [rc/v-box
     :gap "10px"
     :children (for [[item-key item-info] iterable]
       (let [item-path (conj path-prefix item-key)]
       ^{:key (str item-key "-section")}[rc/v-box
       :style {:border "1px solid lightgray"
        :padding "8px"
        :border-radius "4px"
     }
        :children
         [^{:key (str item-key "-header")}[rc/title :label (str (:name item-info) " Select") :level :level2]
         (if (string/includes? (:type item-info) "select")
             (select-buttons item-path item-info redirect)
             (non-button-select item-path item-info iterable)
           )]]
     ))]))

(def tabs-definition
  [{:id :home :label "Home"}
  {:id :about :label "About"}
  {:id :entry :label "Entry"}
  ])

 (defn nav-link [uri title page]
   [:a.navbar-item
    {:href   uri
     :class (when (= page @(rf/subscribe [:common/page-id])) :is-active)}
    title])

(defn change-tab-fn [label]
  (rf/dispatch [ :common/navigate! (if (some #{:home :about} [label]) label :entry)])
)
(defn navbar []
  (let [selected-tab-id @(rf/subscribe [:common/tab-id])
               change-tab change-tab-fn]
              [rc/horizontal-tabs
              :model selected-tab-id
              :tabs tabs-definition
              :on-change change-tab
              ]))

(defn home-page []
  [rc/v-box
    :height "100%"
    :children [
      [rc/title :label "Welcome" :level :level1]
      [rc/button :label "Start Entry" :on-click (on-click-nav :entry)]
    ]])

(defn about-page [_]
  [rc/h-box
  :children[
   [:img {:src "/img/warning_clojure.png"}]
   [rc/p "And Re-com"]]])

(defn all-valid? [info]
 (not (some false? (map (fn [[_ v]] (v :valid?)) info))))

(defn entry-page []
 (let [
   info @(rf/subscribe [:entry-info])
   complete (all-valid? info)
   ]
  [rc/v-box
  :gap "30px"
  :children [
    [rc/title :label "Entry Start" :level :level1]
    (select-area info {:url-key :entry} [:entry-info])
    [rc/button :label "Start Entry" :disabled? (not complete)
    :on-click (on-click-nav :skier-select) ]
       ]]
       ))

 (defn skier-select-page [_]

   (let [
     skiers @(rf/subscribe [:active-skiers])
     all-valid-skiers? (some false?
       (map #(-> % second :attributes all-valid?) skiers))
   ]
   [rc/v-box
   :gap "30px"
   :children [
     [rc/title :label "Active Skier Select" :level :level1]
     [rc/h-box
     :children (for [[key info] skiers]
       ^{:key (str (:name info) "-select")}[rc/button
        :label (:name info)
        :on-click (on-click-nav :skier-gear {:skier-id key})
        :style (rc-toggle-style (all-valid? (:attributes info)))
       ])]
     [rc/button :label "Continue to Runs" :disabled? all-valid-skiers? :on-click (on-click-nav :run-select)]
    ]]))

(defn skier-gear-page [_]

  (let [
    skier-id (keyword @(rf/subscribe [:get-path-param :skier-id]))
    skier-info @(rf/subscribe [:skier-info skier-id])
    valid-selections? (all-valid? (:attributes skier-info))

    path-to-attributes [:entry-info :people :options skier-id :attributes]
    redirect {:url-key :skier-gear :params {:skier-id skier-id}}
  ]
    [rc/v-box
      :gap "10px"
      :children [
        [rc/title :label (str "Gear Selection for " (:name skier-info)) :level :level1]

        (select-area (:attributes skier-info) redirect path-to-attributes)
        [rc/button :label "Add Item Type" :on-click (on-click-nav :add-item
          {:type (string/join "/" (map name (pop path-to-attributes)))
          :key (random-uuid)} redirect)]
        [rc/button :label "Return to Skier Select"  :on-click (on-click-nav :skier-select)
        :style (rc-toggle-style valid-selections? )]
        ]
    ]
  ))

(defn run-vert [grouped type]
  (apply + (map :vert (type grouped)))
  )

(defn run-count [grouped type & args]
  (let [runs (type grouped) [sel] args]
    (if sel
      (count (filter #(= (second sel) ((first sel) %)) runs))
      (count runs))))

(defn run-select-page []
  (let [
    resort-id @(rf/subscribe [:active-resort-id])
    run-opts @(rf/subscribe [:resort-runs])
    {:keys [comment hike-vert-mod] runs-entered :runs :as run-info} @(rf/subscribe [:run-info])
    valid-hike-mod? (integer? (js/parseInt hike-vert-mod))
    grouped-runs (group-by #(-> % :type) (vals runs-entered))

    path-to-attributes [:entry-info :resort :options resort-id :attributes]
    redirect {:url-key :run-select}
    ]
    [rc/v-box
    :children [
      [rc/title :label "Select Runs" :level :level1]
      [rc/h-box
      :gap "5px"
      :children
      [
        [rc/label :label (str "Lifts " (run-count grouped-runs :lift))]
        [rc/line :size  "1px" :color "gray"]
        [rc/label :label (str "Hikes " (run-count grouped-runs :hike))]
        [rc/line :size  "1px" :color "gray"]
        [rc/label :label (str "Runs " (run-count grouped-runs :ski))]
        [rc/line :size  "1px" :color "gray"]
        [rc/label :label (str "Lift Vert " (run-vert grouped-runs :lift))]
        [rc/line :size  "1px" :color "gray"]
        [rc/label :label (str "Hike Vert " (run-vert grouped-runs :hike))]]
      ]
      [rc/v-box
        :children [
          [rc/title :label "Comment" :level :level2]
          [rc/input-text :model comment :change-on-blur? false :on-change (fn [e] (rf/dispatch [:update-run-info :comment e]))]
        ]
      ]
      [rc/v-box
        :children [
          [rc/title :label "Hike Vert Mod" :level :level2]
          [rc/input-text :model (str hike-vert-mod) :change-on-blur? false :on-change (fn [e] (rf/dispatch [:update-run-info :hike-vert-mod (js/parseInt e)]))
          :validation-regex #"-?\d+"]
        ]
      ]
      [rc/v-box
        :children
          (for [type [:hike :lift :ski]]
            (let [info (type run-opts)]
          ^{:key (str type "-section")}[rc/v-box
          :children
          [
          ^{:key (str type "-header")}[rc/title :label (-> type name string/capitalize) :level :level2]
          [rc/h-box
          :gap "7px"
          :children
            (conj (vec (for [[id run-info] (:options info)]
            (let [run-entry (merge
                {:key id :type type :comment comment}
                (select-keys run-info [:name :vert]))]
              ^{:key (str (:name run-info) "-button-and-count")}[rc/h-box
              :gap "3px"
              :children
              [^{:key (str id "-select")}[rc/button
              :label (:name run-info) :on-click (rf-dp-fn :add-run run-entry) :disabled? (not valid-hike-mod?)]

              ^{:key (str (:name run-info) "-Count")}[rc/label :label (run-count grouped-runs type [:key id])]]
              ])))

            ^{:key (str type "-add")}[rc/button :label (str "Add " (name type))
            :on-click (on-click-nav :add-item
              {:type (string/join "/" (map name (conj path-to-attributes type)))
              :key (random-uuid)} redirect)
            :style {:color "white" :background-color "red"}])
            ]]
          ]))]
    [rc/v-box
      :gap "5px"
      :children [
      [rc/title :label "History" :level :level3]
      [rc/v-box :children (for [[num run] runs-entered]
        ^{:key (str (:type run) (:name run) num "-header-entered")}[rc/title :label
          (str (-> run :type name string/capitalize) " " (:name run) " " (:comment run)) :level :level4])]

      [rc/button :label "Delete Last" :on-click (rf-dp-fn :delete-last-run)]
      [rc/line :size "2px" :color "black"]
      [rc/button :label "Submit Observation" :on-click (rf-dp-fn :add-observation) :disabled? (< (count runs-entered) 2)]
      ]
      ]
    ]
    ]
))

 (defn need-vert? [path-seq]
   (some #{:hike :lift} path-seq ))

(defn is-run? [path-seq]
 (some #{:hike :lift :ski} path-seq))

(defn string-to-map [s]
  (as-> s s
    (string/replace s #"[{}:]" "") (string/split s #" ") (map keyword s)
    (partition 2 s) (map vec s) (vec s) (into {} s)))

(defn complete-new-item? [item seq-path]
    (if (need-vert? seq-path)
    (and (> (:vert item) 0) (not= "" (:name item)))
    (not= "" (:name item))))

(defn complete-new-type? [item seq-path]
  (let [item item]
    (and (not= "" (:name item)) (not= "" (:type item)))))

(defn complete-new-check [kind]
  (condp = kind
    :option complete-new-item?
    :attribute complete-new-type?
    (constantly false)))

(defn add-item-page []
  (let [ {:keys [path kind] :as new-info} @(rf/subscribe [:new-entry])
        parent-info @(rf/subscribe [:item-info path])
        {:keys [url-key params query]} @(rf/subscribe [:query-params])
        cljs-query-params (cond-> {:url-key (keyword url-key)}
          params (assoc :params (string-to-map params))
          query (assoc :query (string-to-map query)))
        is-run? (is-run? path)
        need-vert? (need-vert? path)
        complete? ((complete-new-check kind) new-info path)
        inputs (cond-> [[:name :text]]
                 (= :option kind) (conj [:description :text])
                 need-vert? (conj [:vert :number])
                 (= :attribute kind) (concat [[:type ["multiselect" "singleselect"]] [:required [true false]]])
                 )
        submit-path (conj path (-> kind name (str "s") keyword) (-> (random-uuid) str keyword))
  ]
  [rc/v-box
  :gap "10px"
  :children [
  [rc/title :label (str "New " (parent-info :name) (when (= kind :attribute) " Item Type")) :level :level1]
  [rc/v-box
  :gap "10px"
  :children
    (for [[input type] inputs]
      ^{:key (str input "-section")}[rc/v-box
      :style {:border "1px solid lightgray"
       :padding "8px"
       :border-radius "4px"
    }
        :children [
          ^{:key (str input "-title")}[rc/title :label (-> input name string/capitalize) :level :level2 :margin-top "0px"]
          (if (vector? type)
            [rc/h-box
            :gap "10px"
            :children (for [opt type]
              ^{:key (str opt "-select")}[rc/button
                :label (-> opt str string/capitalize)
                :on-click (rf-dp-fn [:update-new input opt])
                :style (rc-toggle-style (= (input new-info) opt))]
                )]
            ^{:key (str input "-input")}[rc/input-text
            :model (str (new-info input))
            :on-change (fn [e]
              (rf/dispatch [:update-new input
                (cond-> e (= type :number) (js/parseInt))]))
            :validation-regex ( if (= type :number) #"\d+" #".+")])]])]
      [rc/h-box
      :gap "10px"
      :children [
        [rc/button :label "Save Item"
        :on-click (rf-dp-fn :save-new submit-path cljs-query-params)
        :disabled? (not complete?)]
        [rc/button :label "Discard Item"
        :on-click (on-click-nav (:url-key cljs-query-params) (:params cljs-query-params) (:query cljs-query-params))
        ]]]]]))

(defn page []
  (if-let [page @(rf/subscribe [:common/page])]
    [:div
     [navbar]
     [page]]))

(defn navigate! [match _]
  (rf/dispatch [:common/navigate match]))

(def router
  (reitit/router
    [["/" {:name        :home
           :view        #'home-page}]
     ["/about" {:name :about
                :view #'about-page}]
     ["/run-select" {:name :run-select
                :view #'run-select-page
                :controllers [{:start (rf-dp-fn :page/init-run-select)}]}]
      ["/entry" {:name :entry
                :view #'entry-page
                :controllers [{:start (rf-dp-fn :get-entry-info)}]}]
      ["/skier-gear/:skier-id" {:name :skier-gear
                      :view #'skier-gear-page
                      }]
      ["/skier-select" {:name :skier-select :view #'skier-select-page}]
      ["/add-item/:type/:key" {:name :add-item
                               :view #'add-item-page
                               :controllers [{
                                 :parameters {:path [:type :key] :query [:url-key :params :query]}
                                 :start (fn [{:keys [path]}]
                                   (rf/dispatch [:add-new-blank-opt (:type path) (:key path)])
                                   )
                                :stop (fn [] (rf/dispatch [:discard-new]))
                               }]}]
                ]))

(defn start-router! []
  (rfe/start!
    router
    navigate!
    {}))

;; -------------------------
;; Initialize app
(defn ^:dev/after-load mount-components []
  (rf/clear-subscription-cache!)
  (rdom/render [#'page] (.getElementById js/document "app")))

(defn init! []
  (start-router!)
  (ajax/load-interceptors!)
  (mount-components))
