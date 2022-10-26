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
    path-to-attributes [:entry-info :people :options skier-id :attributes]
  ]
  [:section
    [:h1 "Gear Selection for " (:name skier-info)]
    (for [[item-key item-info] (:attributes skier-info)]
    (let [path-to-items (conj path-to-attributes item-key)]
    ^{:key (str item-key "-section")}[:section.select-area
      ^{:key (str item-key "-header")}[:h2 (str (:name item-info) " Select")]
      [:section.button-area
        ( for [[opt-key opt-info] (:options item-info)]
        ^{:key (str item-key "-" opt-key)}[:input
                  { :type "button"
                    :value (:name opt-info)
                    :style {:color "white"
                      :background-color (if (:selected opt-info) "green" "red")}
                    :on-click (fn [_] (rf/dispatch [
                      (if (= (:type item-info) "multiselect") :toggle-multi-select :toggle-single-select) path-to-items opt-key]))
          }]
      )
      ^{:key (str item-key "-add")}[:input {
        :type "button" :value (str "Add " (:name item-info))
        :on-click (fn [_]
          (rf/dispatch [:common/navigate! :add-item {:type (string/join "/" (map name path-to-items)) :key (random-uuid)} {:url-key :skier-gear :params {:skier-id skier-id}}])
          )
        }]

      ]
      ]
    ))
    [:h2 "Add Item Type"]
  ]
  )
   ])

(defn skier-select-page [_]
  [:section.section>div.container>div.content
  (let [
    skiers @(rf/subscribe [:active-skiers])
  ]
   [:h1 "You gott here"]
   (for [[key info] skiers]
     ^{:key (str (:name info) "-section")}[:section
     ^{:key (str (:name info) "-select")}[:input {:type :button :value (:name info)
       :on-click (fn [_] (rf/dispatch [:common/navigate! :skier-gear {:skier-id key}]))}]
     ]))
   ])

(defn entry-page []
  (let [
    info @(rf/subscribe [:entry-info])
    complete @(rf/subscribe [:valid-entry?])
    ]
  [:section.section>div.container>div.content
   [:h1 "Entry Start"]

    (for [[item-key item-info] info]
      ^{:key (str item-key "-section")}[:section.select-area
        ^{:key (str item-key "-header")}[:h2 (str (:name item-info) " Select")]
        (if (string/includes? (:type item-info) "select")
          [:section.button-area

            ( for [[opt-key opt-info] (:options item-info)]
            ^{:key (str item-key "-" opt-key)}[:input
                      { :type "button"
                        :value (:name opt-info)
                        :style {:color "white"
                          :background-color (if (:selected opt-info) "green" "red")}
                        :on-click (fn [_] (rf/dispatch [
                          (if (= (:type item-info) "multiselect") :toggle-multi-select :toggle-single-select) item-key opt-key]))
              }]
          )
          ^{:key (str item-key "-add")}[:input {
            :type "button" :value (str "Add " (:name item-info))
            :on-click (fn [_]
              (rf/dispatch [:common/navigate! :add-item {:type item-key :key (random-uuid)} {:url-key :entry}])
              )
            }]
          ]

          ^{:key (str item-key "-input")}[:input {
            :type (:type item-info)
            :on-change (fn [e] (rf/dispatch [:add-entry-db-info item-key (some-> e .-target .-value)]) )
            :value (-> info item-key :selected)
          }])
      ]
    )
    [:section.submit-area [:input (cond->
      {:type "button" :value "Start Entry"
    :on-click (fn [_]
      (rf/dispatch [:common/navigate! :skier-select])
      ) }
      (not complete) (assoc :disabled "disabled")
      )]]]))

(defn home-page []
  [:section.section>div.container>div.content
   (when-let [docs @(rf/subscribe [:docs])]
     [:div {:dangerouslySetInnerHTML {:__html (md->html docs)}}])])

 (defn need-vert? [path-seq]
   (some #{:hike :lift} path-seq )
   )

(defn string-to-map [s]
  (into {}(vec (map vec (partition 2 (map keyword (-> s (string/replace  #"[{}:]" "") (string/split #" ")) ))))))

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
      [:input (cond->
        {:type :button :value "Save Item"
      :on-click (fn [_]
        (rf/dispatch [:save-new path-vec cljs-query-params])
        ) }
        (not complete) (assoc :disabled "disabled")
        )]
      [:input
        {:type :button :value "Discard Item"
      :on-click (fn [_]
        (rf/dispatch [:common/navigate! (:url-key cljs-query-params) (:params cljs-query-params) (:query cljs-query-params)])
        )}]
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
           :controllers [{:start (fn [_] (rf/dispatch [:page/init-home]))}]}]
     ["/about" {:name :about
                :view #'about-page}]
      ["/entry" {:name :entry
                :view #'entry-page
                :controllers [{:start (fn [_] (rf/dispatch [:get-entry-info]))}]}]
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
