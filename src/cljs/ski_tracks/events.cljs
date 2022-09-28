(ns ski-tracks.events
  (:require
    [re-frame.core :as rf]
    [ajax.core :as ajax]
    [reitit.frontend.easy :as rfe]
    [reitit.frontend.controllers :as rfc]))

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

(defn select-options-false [h]
  (into {}
    (map
    (fn [[k v]] [k (assoc v :selected false)])
    h))
)

(defn selected-false
  ([h] (selected-false h {}))
  ([arrs to-ret]
    (if (empty? arrs)
      to-ret
      (let [[k v] (first arrs)]
        (if (:options v)
          (recur (rest arrs) (assoc to-ret k
            (assoc v :options
              (into {} (map
                (fn [[k2 v2]]
                  [k2
                    (if (:attributes v2)
                      (assoc v2 :attributes (selected-false (:attributes v2)))
                      v2)])
                (:options (assoc v :options (select-options-false (:options v)))))))))
          (recur (rest arrs) (assoc to-ret k v)))))))

(rf/reg-event-db
  :set-entry-info
  (fn [db [_ info]]

    (assoc db :entry-info (selected-false info)))
)

(rf/reg-event-db
  :set-bad-entry-info
  (fn [db _]
    (assoc db :entry-info "NO")))

(rf/reg-event-fx
  :get-entry-info
  (fn [_ _]
    {:http-xhrio {:method          :get
                  :uri             "/api/entry-info"
                  :response-format (ajax/json-response-format {:keywords? true})
                  :on-success       [:set-entry-info]
                  :on-failure       [:set-bad-entry-info]}}))

(rf/reg-event-db
  :common/set-error
  (fn [db [_ error]]
    (assoc db :common/error error)))

(rf/reg-event-fx
  :page/init-home
  (fn [_ _]
    {:dispatch [:fetch-docs]}))

(rf/reg-event-db
  :toggle-multi-select
  (fn [db [_ opt k]]
    (assoc-in db [:entry-info opt :options k :selected] (-> db :entry-info opt :options k :selected not))
    ))

(rf/reg-event-db
  :toggle-single-select
  (fn [db [_ opt toggle_key]]
    (assoc-in db [:entry-info opt :options]
      (into {}
        (map (fn [[k v]]
          [k (if (= k toggle_key)
                (assoc v :selected (-> v :selected not))
                (assoc v :selected false)
          )]
          )
          (-> db :entry-info opt :options)
        )))
    ))

(rf/reg-event-db
  :add-entry-info
  (fn [db [_ k v]]
    (assoc-in db [:entry-info k :selected] v)
    ))
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

(rf/reg-sub
  :common/error
  (fn [db _]
    (:common/error db)))
