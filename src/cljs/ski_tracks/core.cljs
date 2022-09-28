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

(defn about-page []
  [:section.section>div.container>div.content
   [:img {:src "/img/warning_clojure.png"}]])

(defn entry-page []
  [:section.section>div.container>div.content
   [:h1 "Entry Start"]

   (when-let [info @(rf/subscribe [:entry-info])]
    (for [[item-key item-info] info]
      ^{:key (str item-key "-section")}[:section.select-area
        ^{:key (str item-key "-header")}[:h2 (str (:name item-info) " Selects")]
        (if (string/includes? (:type item-info) "select")
          ( for [[opt-key opt-info] (:options item-info)]
            ^{:key (str item-key "-" opt-key)}[:input
                      { :type "button"
                        :value (:name opt-info)
                        :style {:color "white" :background-color (if (:selected opt-info) "green" "red")}
                        :on-click (fn [_] (rf/dispatch [
                          (if (= (:type item-info) "multiselect") :toggle-multi-select :toggle-single-select) item-key opt-key]))
              }]
          )

          ^{:key (str item-key "-input")}[:input {
            :type (:type item-info)
            :on-change #( (rf/dispatch [:add-entry-info item-key (-> % .-target .-value)]) )
          }])
      ]
    )


    ;  (for [[k person-info] (-> info :people :options)]
    ;  ^{:key (str "person-" k)}
    ;     [:input { :type "button"
    ;               :value (:name person-info)
    ;               :style {:color "white" :background-color (if (:selected person-info) "green" "red")}
    ;               :on-click (fn [_] (rf/dispatch [:toggle-person-select k]))
    ;    }])
    ; [:h1 "Skier Select"]
    ; [:h2 "Resoasdrt Select"][:h3 "asdg"]
    )
   ])

(defn home-page []
  [:section.section>div.container>div.content
   (when-let [docs @(rf/subscribe [:docs])]
     [:div {:dangerouslySetInnerHTML {:__html (md->html docs)}}])])

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
