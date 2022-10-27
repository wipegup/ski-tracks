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

(defn rf-dp-fn [& args]
  (let [args (if (sequential? (first args)) (first args) args)]
  (fn [_]
    (rf/dispatch (vec args))
  )))

(defn on-click-nav [& args]
  (rf-dp-fn (vec (cons :common/navigate! args))))

(defn toggle-button [value toggle on-click key]
  ^{:key key}[:input {:type :button :value value
    :style {:color "white"
      :background-color (if toggle "green" "red")}
    :on-click on-click}])

(defn disable-button [value toggle on-click]
  [:input (cond->
    {:type :button :value value
    :on-click on-click}
    toggle (assoc :disabled "disabled")
  )])

 (defn select-buttons [item-path item-info redirect]
   [:section.button-area
   ( for [[opt-key opt-info] (:options item-info)]
   (toggle-button
    (:name opt-info)
    (:selected opt-info)
    (rf-dp-fn (if (= (:type item-info) "multiselect") :toggle-multi-select :toggle-single-select) item-path opt-key)
    (str item-path "-" opt-key)))
 ^{:key (str item-path "-add")}[:input {
   :type "button" :value (str "Add " (:name item-info))
   :on-click (on-click-nav :add-item {:type (string/join "/" (map name item-path)) :key (random-uuid)} redirect)
     }]])

   (defn non-button-select [item-path item-info info]
     ^{:key (str item-path "-input")}[:input {
       :type (:type item-info)
       :on-change (fn [e] (rf/dispatch [:add-entry-db-info item-path (some-> e .-target .-value)]) )
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

(defn skier-gear-page [_]
  [:section.section>div.container>div.content
  (let [
    path-params @(rf/subscribe [:path-params])
    skier-id (keyword (:skier-id path-params))
    skier-info @(rf/subscribe [:skier-info skier-id])
    valid-selections? @(rf/subscribe [:valid-skier-selections? skier-id])
    path-to-attributes [:entry-info :people :options skier-id :attributes]
    redirect {:url-key :skier-gear :params {:skier-id skier-id}}
  ]
  [:section
    [:h1 "Gear Selection for " (:name skier-info)]

    (select-area (:attributes skier-info) redirect path-to-attributes)

    [:section [:input {:type :button :value "Add Item Type"
    :on-click (on-click-nav :add-item-type {:type (string/join "/" (map name path-to-attributes)) :key (random-uuid)} redirect)
    }]]

    (toggle-button "Return to Skier Select" valid-selections? (on-click-nav :skier-select) "ret")
  ]
  )
   ])

(defn skier-select-page [_]
  [:section.section>div.container>div.content
  (let [
    skiers @(rf/subscribe [:active-skiers])
    all-valid? (not @(rf/subscribe [:active-skier-valid-selections?]))
  ]
   [:h1 "You gott here"]
   [:section
   (for [[key info] skiers]
     ^{:key (str (:name info) "-section")}[:section
      (toggle-button
        (:name info)
        @(rf/subscribe [:valid-skier-selections? key])
        (on-click-nav :skier-gear {:skier-id key})
        (str (:name info) "-select")
        )
     ])
     (disable-button "Continue to Runs" all-valid? (on-click-nav :run-select))
       ]
     )])

(defn run-select-page []
  [:section.section>div.container>div.content
  (let [
    resort-id @(rf/subscribe [:active-resort-id])
    run-opts @(rf/subscribe [:resort-runs resort-id])
    runs-entered @(rf/subscribe [:runs-entered])
    comment @(rf/subscribe [:run-comment])
    hike-vert-mod @(rf/subscribe [:hike-vert-mod])
    valid-hike-vert-mod @(rf/subscribe [:valid-hike-vert-mod])
    valid-hike-mod @(rf/subscribe [:valid-hike-vert-mod])
    lift-count @(rf/subscribe [:run-count :lift])
    hike-count @(rf/subscribe [:run-count :hike])
    ski-count @(rf/subscribe [:run-count :ski])
    lift-vert @(rf/subscribe [:up-vert :lift])
    hike-vert @(rf/subscribe [:up-vert :hike])
    path-to-attributes [:entry-info :resort :options resort-id :attributes]
    redirect {:url-key :run-select}
    ]
    [:section.show
    [:h2 "Select Runs"]
    [:span (str "Lifts " lift-count)]
    [:span (str "Hikes " hike-count)]
    [:span (str "Runs " ski-count)]
    [:span (str "Lift Vert " lift-vert)]
    [:span (str "Hikes Vert " hike-vert)]
  [:section.comment
    [:h3 "Comments"]
    [:input {:type :text :name :comments
      :on-change (fn [e]
        (rf/dispatch [:update-run-comment (some-> e .-target .-value)]))
      :value comment}]
  ]
  [:section.hike-vert
        [:h3 "Hike Vert Mod"]
        [:input {:type :text :name :hike-vert-mod
                 :on-change (fn [e]
                             (rf/dispatch [:update-hike-vert-mod  (some-> e .-target .-value)]))
                 :value hike-vert-mod}]

        (when (not valid-hike-vert-mod) [:p "Mod must be Int"])]
  [:section.opts

  (for [[type info] run-opts]
    ^{:key (str type "-section")}[:section.type
    ^{:key (str type "-header")}[:h2 (-> type name string/capitalize)]
      (for [[id run-info] (:options info)]
      ^{:key (str id "-select")}[:input
      {:type :button :value (:name run-info)
      :on-click (rf-dp-fn :add-run (merge {:key id :type type :comment comment} (select-keys run-info [:name :vert])))}]
      )
      ^{:key (str type "-add")}[:input
      {:type :button :value (str "Add " (name type))
      :style {:color "white"
        :background-color "red"}
      :on-click (on-click-nav :add-item {:type (string/join "/" (map name (conj path-to-attributes type))) :key (random-uuid)} redirect)
      }]
    ]
    )
    ]
    [:section.done
  (for [[num run] runs-entered]
    ; ^{:key (str (:type run) (:name run) idx "-section-entered")}
    ^{:key (str (:type run) (:name run) num "-header-entered")}[:div (str (-> run :type name string/capitalize) " " (:name run) " " (:comment run)) ]
    )
    [:input {:type :button :value "Delete Last" :on-click (rf-dp-fn :delete-last-run)}]
    ]
    ]
  )
  ]
  )

(defn entry-page []
  (let [
    info @(rf/subscribe [:entry-info])
    complete @(rf/subscribe [:valid-entry?])
    ]
  [:section.section>div.container>div.content
   [:h1 "Entry Start"]
    (select-area info {:url-key :entry})
    [:section.submit-area
      (disable-button "Start Entry" (not complete) (on-click-nav :skier-select))
        ]]))

(defn home-page []
  [:section.section>div.container>div.content
   (when-let [docs @(rf/subscribe [:docs])]
     [:div {:dangerouslySetInnerHTML {:__html (md->html docs)}}])])

 (defn need-vert? [path-seq]
   (some #{:hike :lift} path-seq ))

(defn string-to-map [s]
  (into {}(vec (map vec (partition 2 (map keyword (-> s (string/replace  #"[{}:]" "") (string/split #" ")) ))))))

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
            (rf/dispatch [:update-new (conj path-vec :name) (some-> e .-target .-value)]))
          :value (:name item-info)
            }]
    ]
      [:section.type
        [:h3 "Type"]
          (toggle-button "Multi-Select" (= (:type item-info) "multiselect")
            (rf-dp-fn [:update-new (conj path-vec :type) "multiselect"]) "multiselect")
          (toggle-button "Single-Select" (= (:type item-info) "singleselect")
            (rf-dp-fn [:update-new (conj path-vec :type) "singleselect"]) "singleselect")
      ]
      [:section.required
        [:h3 "Required?"]
          (toggle-button "True" (:required item-info)
            (rf-dp-fn [:update-new (conj path-vec :required) true]) "req-true")
          (toggle-button "False" (not (:required item-info))
            (rf-dp-fn [:update-new (conj path-vec :required) false]) "req-false")
      ]
      [:section.submit
        (disable-button "Add Item Type" (not complete)
        (rf-dp-fn :save-new path-vec cljs-query-params))
      ]])])

(defn add-item-page []
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
      complete @(rf/subscribe [:complete-new-item? path-vec])
      inputs (cond-> [[:name :text] [:description :text]]
               (need-vert? path-vec) (conj [:vert :number]))
      ]
    [:div
    [:h2 "New " (parent-info :name)]

    (for [[input type] inputs]
      ^{:key (str input "-section")}[:section
        ^{:key (str input "-title")}[:h3 (-> input name string/capitalize)]
        ^{:key (str input "-input")}[:input {:type type :name input
          :on-change (fn [e]
            (rf/dispatch [:update-new (conj path-vec input) (some-> e .-target .-value)]))
          }]])

    [:section.submit-area
      (disable-button "Save Item" (not complete)
        (rf-dp-fn :save-new path-vec cljs-query-params))
      [:input
        {:type :button :value "Discard Item"
      :on-click (on-click-nav (:url-key cljs-query-params) (:params cljs-query-params) (:query cljs-query-params))}]
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
