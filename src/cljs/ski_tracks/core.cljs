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
    [clojure.string :as string])
  (:import goog.History))

(defn nav-link [uri title page]
  [:a.navbar-item
   {:href   uri
    :class (when (= page @(rf/subscribe [:common/page-id])) :is-active)}
   title])

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
   (let [{:keys [added-opts] :or {added-opts {}}} (if (-> args last map?) (last args) {})
          args (if (-> args last map?) (butlast args) args)
          [text add-kind redirect add-path key] args
     on-click (on-click-nav add-kind
       {:type (string/join "/" (map name add-path))
       :key (random-uuid)} redirect)]
     (button text on-click added-opts key)))

 (defn select-buttons [item-path item-info redirect]
   [:section.button-area
   ( for [[opt-key opt-info] (:options item-info)]
   (toggle-button
    (:name opt-info)
    (rf-dp-fn (if (= (:type item-info) "multiselect") :toggle-multi-select :toggle-single-select) item-path opt-key)
    (:selected opt-info)
    (str item-path "-" opt-key)))
 ^{:key (str item-path "-add")}[:input {
   :type "button" :value (str "Add " (:name item-info))
   :on-click (on-click-nav :add-item {:type (string/join "/" (map name item-path)) :key (random-uuid)} redirect)
     }]])

   (defn non-button-select [item-path item-info info]
     ^{:key (str item-path "-input")}[:input {
       :type (:type item-info)
       :on-change (fn [e] (rf/dispatch [:add-entry-db-info item-path (event-val e)]) )
       :value (get-in info (conj (vec item-path) :selected))
     }]
     )

   (defn select-area
     ([iterable redirect] (select-area iterable redirect []))
     ([iterable redirect path-prefix]
       (for [[item-key item-info] iterable]
         (let [item-path (conj path-prefix item-key)]
         ^{:key (str item-key "-section")}[:section.select-area
           ^{:key (str item-key "-header")}[:h2 (str (:name item-info) " Select")]
           (if (string/includes? (:type item-info) "select")
               (select-buttons item-path item-info redirect)
               (non-button-select item-path item-info iterable)
             )]
       ))))

(defn navbar []
  (r/with-let [expanded? (r/atom false)]
              [:nav.navbar.is-info>div.container
               [:div.navbar-brand
                [:a.navbar-item {:href "/" :style {:font-weight :bold}} "ski-tracks"]
                [:span.navbar-burger.burger
                 {:data-target :nav-menu
                  :on-click #(swap! expanded? not)
                  :class (when @expanded? :is-active)}
                 [:span][:span][:span]]]
               [:div#nav-menu.navbar-menu
                {:class (when @expanded? :is-active)}
                [:div.navbar-start
                 [nav-link "#/" "Home" :home]
                 [nav-link "#/about" "About" :about]
                 [nav-link "#/entry" "Entry" :entry]
                 ]]]))

(defn about-page [_]
  [:section.section>div.container>div.content
   [:img {:src "/img/warning_clojure.png"}]])

(defn all-valid? [info]
 (not (some false? (map (fn [[_ v]] (v :valid?)) info))))

(defn entry-page []
 (let [
   info @(rf/subscribe [:entry-info])
   complete @(rf/subscribe [:valid-entry?])
   ]
 [:section.section>div.container>div.content
  [:h1 "Entry Start"]
   (select-area info {:url-key :entry})
   [:section.submit-area
     (disable-button "Start Entry" (on-click-nav :skier-select) (not complete))
       ]]))

 (defn skier-select-page [_]
   [:section.section>div.container>div.content
   (let [
     skiers @(rf/subscribe [:active-skiers])
     all-valid-skiers? (some false?
       (map #(-> % second :attributes all-valid?) skiers))
   ]
    [:section.body
      [:section.title
       [:h1 "Active Skier Select"]
      ]
    [:section.button-area
    (for [[key info] skiers]
       (toggle-button (:name info)
         (on-click-nav :skier-gear {:skier-id key})
         (all-valid? (:attributes info))
         (str (:name info) "-select"))
     )]
    [:section.submit
      (disable-button "Continue to Runs"
       (on-click-nav :run-select) all-valid-skiers?)
    ]
    ])])

(defn skier-gear-page [_]
  [:section.section>div.container>div.content
  (let [
    skier-id (keyword @(rf/subscribe [:get-path-param :skier-id]))
    skier-info @(rf/subscribe [:skier-info skier-id])
    valid-selections? (all-valid? (:attributes skier-info))

    path-to-attributes [:entry-info :people :options skier-id :attributes]
    redirect {:url-key :skier-gear :params {:skier-id skier-id}}
  ]
  [:section.body
    [:section.title
      [:h1 "Gear Selection for " (:name skier-info)]
    ]
    (select-area (:attributes skier-info) redirect path-to-attributes)
    [:section.submit
      (add-button "Add Item Type" :add-item-type redirect path-to-attributes)
      (toggle-button "Return to Skier Select" (on-click-nav :skier-select) valid-selections? "ret")
    ]]
  )])

(defn run-vert [grouped type]
  (apply + (map :vert (type grouped)))
  )

(defn run-count [grouped type & args]
  (let [runs (type grouped) [sel] args]
    (if sel
      (count (filter #(= (second sel) ((first sel) %)) runs))
      (count runs))))

(defn run-select-page []
  [:section.section>div.container>div.content
  (let [
    resort-id @(rf/subscribe [:active-resort-id])
    run-opts @(rf/subscribe [:resort-runs])
    {:keys [comment hike-vert-mod] runs-entered :runs :as run-info} @(rf/subscribe [:run-info])
    valid-hike-mod? (integer? (js/parseInt hike-vert-mod))
    grouped-runs (group-by #(-> % :type) (vals runs-entered))

    path-to-attributes [:entry-info :resort :options resort-id :attributes]
    redirect {:url-key :run-select}
    ]
    [:section.body
      [:section.title [:h1 "Select Runs"]]
      [:section.totals
        [:span (str "Lifts " (run-count grouped-runs :lift))]
        [:span (str "Hikes " (run-count grouped-runs :hike))]
        [:span (str "Runs " (run-count grouped-runs :ski))]
        [:span (str "Lift Vert " (run-vert grouped-runs :lift))]
        [:span (str "Hike Vert " (run-vert grouped-runs :hike))]
      ]
      [:section.entry
        (for [[kw value] [[:comment comment] [:hike-vert-mod hike-vert-mod]]]
          (let [
            section (keyword (str "section." (name kw)))
            title (as-> kw k (name k) (string/split k #"-" )
              (map string/capitalize k) (string/join " " k))]

            ^{:key (str kw "-section")}[section
              ^{:key (str kw "-title")}[:h3 title]
              ^{:key (str kw "-input")}[:input {:type :text :name kw :on-change (fn [e]
                (rf/dispatch [:update-run-info kw (event-val e)]))
                :value value}]]))

        (when (not valid-hike-mod?) [:h4 "Hike Mod must be Int"])
      [:section.run-select
        (for [[type info] run-opts]
          ^{:key (str type "-section")}[:section.type
          ^{:key (str type "-header")}[:h3 (-> type name string/capitalize)]
            (for [[id run-info] (:options info)]
            (let [run-entry (merge
                {:key id :type type :comment comment}
                (select-keys run-info [:name :vert]))]
              ^{:key (str (:name run-info) "-button-and-count")}[:span.button-and-count
              (disable-button (:name run-info) (rf-dp-fn :add-run run-entry)
                (not valid-hike-mod?) (str id "-select"))
              ^{:key (str (:name run-info) "-Count")}[:span (run-count grouped-runs type [:key id])]]))

            (add-button (str "Add " (name type)) :add-item
              redirect path-to-attributes (str type "-add")
              {:added-opts {:style {:color "white" :background-color "red"}}})
          ])]]
    [:section.runs-completed
      (for [[num run] runs-entered]
        ^{:key (str (:type run) (:name run) num "-header-entered")}[:div
          (str (-> run :type name string/capitalize) " " (:name run) " " (:comment run))])
      [:input {:type :button :value "Delete Last" :on-click (rf-dp-fn :delete-last-run)}]]]
)])

(defn home-page []
  [:section.section>div.container>div.content
   [:h1 "Welcome"]
   (button "Start Entry" (on-click-nav :entry))])

 (defn need-vert? [path-seq]
   (some #{:hike :lift} path-seq ))

(defn string-to-map [s]
  (into {}(vec (map vec (partition 2 (map keyword (-> s (string/replace  #"[{}:]" "") (string/split #" ")) ))))))

(defn add-item-page []
  [:section.section>div.container>div.content
  (let [
      {:keys [path-vec]} @(rf/subscribe [:path-params])
      {:keys [url-key params query]} @(rf/subscribe [:query-params])
      cljs-query-params (cond-> {:url-key (keyword url-key)}
        params (assoc :params (string-to-map params))
        query (assoc :query (string-to-map query)))

      parent-info @(rf/subscribe [:parent-info path-vec])
      item-info @(rf/subscribe [:item-info path-vec])
      complete @(rf/subscribe [:complete-new-item? path-vec])
      inputs (cond-> [[:name :text] [:description :text]]
               (need-vert? path-vec) (conj [:vert :number]))]
    [:div
    [:h2 "New " (parent-info :name)]

    (for [[input type] inputs]
      ^{:key (str input "-section")}[:section
        ^{:key (str input "-title")}[:h3 (-> input name string/capitalize)]
        ^{:key (str input "-input")}[:input {:type type :name input
          :on-change (fn [e]
            (rf/dispatch [:update-new (conj path-vec input)
            (cond-> (event-val e)
            (= type :number) (js/parseInt)
            )]))
          }]])

    [:section.submit-area
      (disable-button "Save Item"
        (rf-dp-fn :save-new path-vec cljs-query-params) (not complete))
      [:input
        {:type :button :value "Discard Item"
      :on-click (on-click-nav (:url-key cljs-query-params) (:params cljs-query-params) (:query cljs-query-params))}]
    ]])])

  (defn add-item-type-page []
    [:section.section>div.container>div.content
    (let [
        path-params @(rf/subscribe [:path-params])
        query-params @(rf/subscribe [:query-params])
        cljs-query-params
        (cond-> {
            :url-key (keyword (:url-key query-params))
          }
          (:params query-params) (assoc :params  ( string-to-map (:params query-params)))
          (:query query-params) (assoc :query (string-to-map (:query query-params)))
        )

        path-vec (:path-vec path-params)
        parent-info @(rf/subscribe [:parent-info path-vec])
        item-info @(rf/subscribe [:item-info path-vec])
        complete @(rf/subscribe [:complete-new-type? path-vec])
        ]
        [:section
        [:section.name
          [:h3 "Name"]
          [:input {:type :text :name :name
            :on-change (fn [e]
              (rf/dispatch [:update-new (conj path-vec :name) (event-val e)]))
            :value (:name item-info)
              }]
      ]
        [:section.type
          [:h3 "Type"]
            (toggle-button "Multi-Select"
              (rf-dp-fn [:update-new (conj path-vec :type) "multiselect"])
              (= (:type item-info) "multiselect") "multiselect")
            (toggle-button "Single-Select"
              (rf-dp-fn [:update-new (conj path-vec :type) "singleselect"])
              (= (:type item-info) "singleselect") "singleselect")
        ]
        [:section.required
          [:h3 "Required?"]
            (toggle-button "True"
              (rf-dp-fn [:update-new (conj path-vec :required) true]) (:required item-info) "req-true")
            (toggle-button "False"
              (rf-dp-fn [:update-new (conj path-vec :required) false]) (not (:required item-info)) "req-false")
        ]
        [:section.submit
          (disable-button "Add Item Type"
          (rf-dp-fn :save-new path-vec cljs-query-params) (not complete))
        ]])])

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
           :view        #'home-page
           :controllers [{:start (rf-dp-fn :page/init-home)}]}]
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
                                :stop (fn [{:keys [path query]}]
                                  (rf/dispatch [:save-or-discard-opt (:type path) (:key path)])
                                  )
                               }]}]
      ["/add-item-type/:type/:key" {:name :add-item-type
                               :view #'add-item-type-page
                               :controllers [{
                                 :parameters {:path [:type :key] :query [:url-key :params :query]}

                                 :start (fn [{:keys [path]}]
                                   (rf/dispatch [:add-new-blank (:type path) (:key path) :type])
                                   )
                                :stop (fn [{:keys [path query]}]
                                  (rf/dispatch [:save-or-discard-type (:type path) (:key path)])
                                  )
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
