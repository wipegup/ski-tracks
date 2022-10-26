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
   (defn select-buttons [item-path item-info redirect]
     [:section.button-area
     ( for [[opt-key opt-info] (:options item-info)]
     ^{:key (str item-path "-" opt-key)}[:input
               { :type "button"
                 :value (:name opt-info)
                 :style {:color "white"
                   :background-color (if (:selected opt-info) "green" "red")}
                 :on-click (fn [_] (rf/dispatch [
                   (if (= (:type item-info) "multiselect") :toggle-multi-select :toggle-single-select) item-path opt-key]))
       }]
   )
   ^{:key (str item-path "-add")}[:input {
     :type "button" :value (str "Add " (:name item-info))
     :on-click (fn [_]
       (rf/dispatch [:common/navigate! :add-item {:type (string/join "/" (map name item-path)) :key (random-uuid)} redirect])
       )
     }]
     ]
     )

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
    path-to-attributes [:entry-info :people :options skier-id :attributes]
    valid-selections? @(rf/subscribe [:valid-skier-selections? skier-id])
  ]
  [:section
    [:h1 "Gear Selection for " (:name skier-info)]

    (select-area (:attributes skier-info) {:url-key :skier-gear :params {:skier-id skier-id}} path-to-attributes)
    [:h2 "Add Item Type"]
    [:input {:type :button :value "Return to Skier Select"
    :on-click (fn [_] (rf/dispatch [:common/navigate! :skier-select]))
    :style {:color "white"
      :background-color (if valid-selections? "green" "red")}
    }]
  ]
  )
   ])

(defn skier-select-page [_]
  [:section.section>div.container>div.content
  (let [
    skiers @(rf/subscribe [:active-skiers])
  ]
   [:h1 "You gott here"]
   [:section
   (for [[key info] skiers]
     ^{:key (str (:name info) "-section")}[:section
     ^{:key (str (:name info) "-select")}[:input {:type :button :value (:name info)
       :on-click (fn [_] (rf/dispatch [:common/navigate! :skier-gear {:skier-id key}]))
       :style {:color "white"
         :background-color (if @(rf/subscribe [:valid-skier-selections? key]) "green" "red")}
       }]
     ])
     [:input (cond-> {:type :button :value "Continue to Runs"
     :on-click (fn [_] (rf/dispatch [:common/navigate! :run-select]))
     }
     (not @(rf/subscribe [:active-skier-valid-selections?])) (assoc :disabled "disabled")
     )]
       ]
     )
   ])

(defn run-select-page []
  [:section

  [:h1 "YEa"]]
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
      [:input (cond->
        {:type "button" :value "Start Entry"
          :on-click (fn [_]
        (rf/dispatch [:common/navigate! :skier-select])
        )}
        (not complete) (assoc :disabled "disabled"))]]]))

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
     ["/run-select" {:name :run-select
                :view #'run-select-page}]
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
