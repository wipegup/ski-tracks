(ns ski-tracks.events
  (:require
    [re-frame.core :as rf]
    [ajax.core :as ajax]
    [reitit.frontend.easy :as rfe]
    [reitit.frontend.controllers :as rfc]
    [clojure.string :as string]))

;;dispatchers

(rf/reg-event-db
  :common/navigate
  (fn [db [_ match]]
    (let [old-match (:common/route db)
          new-match (assoc match :controllers
                                 (rfc/apply-controllers (:controllers old-match) match))]
      (assoc db :common/route new-match))))

(rf/reg-fx
  :common/navigate-fx!
  (fn [[k & [params query]]]
    (rfe/push-state k params query)))

(rf/reg-event-fx
  :common/navigate!
  (fn [_ [_ url-key params query]]
    {:common/navigate-fx! [url-key params query]}))

(defn alert-string [info]
  (str
    "Not enought"
    ; (map (fn [[_ v]]
    ;   (if (:required v)
    ;     (if (string/includes? (:type item-info) "select")
    ;
    ;     )
    ;     nil
    ;   )
    ;   ))
  )
)

(rf/reg-fx
 :show-alert
 (fn [message]
   (js/alert (str "I was asked to print this: " message))))


(rf/reg-event-db
  :set-docs
  (fn [db [_ docs]]
    (assoc db :docs docs)))

(rf/reg-event-fx
  :fetch-docs
  (fn [_ _]
    {:http-xhrio {:method          :get
                  :uri             "/docs"
                  :response-format (ajax/raw-response-format)
                  :on-success       [:set-docs]}}))


(rf/reg-event-db
  :set-entry-info
  (fn [db [_ info]]
    (assoc db :entry-info info))
)

(rf/reg-event-db
  :set-bad-entry-info
  (fn [db _]
    (assoc db :entry-info "NO")))

(rf/reg-event-fx
  :get-entry-info
  (fn [cofx _]
    (when (not (-> cofx :db :entry-info))
    {:http-xhrio {:method          :get
                  :uri             "/api/entry-info"
                  :response-format (ajax/json-response-format {:keywords? true})
                  :on-success       [:set-entry-info]
                  :on-failure       [:set-bad-entry-info]}})))

(rf/reg-event-db
  :common/set-error
  (fn [db [_ error]]
    (assoc db :common/error error)))

(rf/reg-event-fx
  :page/init-home
  (fn [_ _]
    {:dispatch [:fetch-docs]}))

(defn root-path [path]
  (let [seq-path (if (sequential? path) path [path])]
   (into [] (cons :entry-info seq-path))))

(defn extend-path [path & ext]
  (into [] (concat (root-path path) ext)))

(defn selected-path
  ([path opt-key] (extend-path path :options opt-key :selected))
  ([path] (extend-path path :selected))
  )

(defn valid-path [path]
  (extend-path path :valid?))

(defn options-path [path]
  (extend-path path :options))

(defn required-path [path]
  (extend-path path :required))

(defn valid-select? [db path]
  (let [root (root-path path)]
    (boolean (or (not (get-in db
      (required-path path)))
    (some true?
    (map (fn [[_ v]] (v :selected)) (get-in db (options-path path)) )
  ) ) ))
  )

(rf/reg-event-db
  :toggle-multi-select
  (fn [db [_ path opt-key]]
    (let [
      selected-path (selected-path path opt-key)
      tog
          (assoc-in db selected-path (not (get-in db selected-path)))
      ]
          (assoc-in tog (valid-path path) (valid-select? tog path))
        )
    ))

(rf/reg-event-db
  :toggle-single-select
  (fn [db [_ path toggle_key]]
    (let [
      options-path (options-path path)
      tog (assoc-in db options-path
      (into {}
        (map (fn [[k v]]
          [k (if (= k toggle_key)
                (assoc v :selected (-> v :selected not))
                (assoc v :selected false)
          )]
          )
          (get-in db options-path)
        )))
        ]
        (assoc-in tog (valid-path path) (valid-select? tog path)))))

(rf/reg-event-db
  :add-entry-db-info
  (fn [db [_ path v]]
    (let [tog (assoc-in db (selected-path path) v)]
      (assoc-in tog (valid-path path) true))))

;;subscriptions

(rf/reg-sub
  :common/route
  (fn [db _]
    (-> db :common/route)))

(rf/reg-sub
  :common/page-id
  :<- [:common/route]
  (fn [route _]
    (-> route :data :name)))

(rf/reg-sub
  :common/page
  :<- [:common/route]
  (fn [route _]
    (-> route :data :view)))

(rf/reg-sub
  :docs
  (fn [db _]
    (:docs db)))

(rf/reg-sub
  :entry-info
  (fn [db _]
    (:entry-info db)))

(defn all-valid? [info]
  (not (some false? (map (fn [[_ v]] (v :valid?)) info))))

(rf/reg-sub
  :valid-entry?
  :<- [:entry-info]
  (fn [info]
    (all-valid? info)
    )
  )

(rf/reg-sub
  :common/error
  (fn [db _]
    (:common/error db)))
