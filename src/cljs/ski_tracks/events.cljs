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
  (if (= :entry-info (first seq-path))
    (vec seq-path)
    (vec (cons :entry-info seq-path)))
   ))

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

(defn selected-option-path [path key]
  (extend-path (options-path path) key))

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

;; For Testing
; (rf/reg-event-db
;   :add-arbitrary
;   (fn [db [_ k v]]
;     (assoc db k v)))

; "people/options/28fdeace-3015-4433-8678-6c4c1583290b/attributes/jackets"
; "resort/options/b0c38ab8-47a9-4ca1-b8cd-4ceaffe67d95/attributes/hike"

(defn get-keyword-path [path-string]
  (vec (root-path (map #(keyword %) (string/split path-string #"/")))))

(def simple-blank
  {:name "" :description "" :selected false :attributes {}})

(def type-blank
  {:type "" :required true :name "" :options {} :valid? false})

(rf/reg-event-db
  :add-new-blank-opt
  (fn [db [_ type uuid]]
    (let [
      init-path (get-keyword-path type)
      path (selected-option-path init-path (keyword uuid))
      with-path (assoc-in db [:common/route :path-params :path-vec] path)
      ]
    (assoc-in with-path path simple-blank)
    )))

(rf/reg-event-db
  :add-new-blank
  (fn [db [_ type uuid blank-type]]
    (let [
      init-path (get-keyword-path type)
      path (extend-path init-path (keyword uuid))
      with-path (assoc-in db [:common/route :path-params :path-vec] path)
      ]
    (assoc-in with-path path (if (= blank-type :type) type-blank simple-blank))
    )
    )
  )
(rf/reg-event-db
  :add-new-blank-type
  (fn [])
  )

(defn need-vert? [path-seq]
  (some #{:hike :lift} path-seq))

(defn complete-new-item? [db seq-path]
  (let [item (get-in db seq-path)]
    (if (need-vert? seq-path)
    (and (< 0 (:vert item)) (not= "" (:name item)))
    (not= "" (:name item)))))

(rf/reg-event-fx
  :save-or-discard-opt
  (fn [{:keys [db] :as cofx} [_ type uuid]]
    (let [
      init-path (get-keyword-path type)
      path (selected-option-path init-path (keyword uuid))
      entry (get-in db path)]
      (if (and (:saved entry) (complete-new-item? db path))
        {:db (update-in db path dissoc :saved)
          ; :fx [[:dispatch [:update-data path]]]
        }
        {:db (update-in db (pop path) dissoc (peek path))}
      ))))


(defn complete-new-type? [db seq-path]
  (let [item (get-in db seq-path)]
    (and (not= "" (:name item)) (not= "" (:type item)))))

(rf/reg-event-fx
  :save-or-discard-type
  (fn [{:keys [db] :as cofx} [_ type uuid]]
    (let [
      init-path (get-keyword-path type)
      path (extend-path init-path (keyword uuid))
      entry (get-in db path)]
      (if (and (:saved entry) (complete-new-type? db path))
        (let [tog (update-in db path dissoc :saved)]
        {:db (assoc-in db (conj path :valid?) (not (get-in db (conj path :required))))
          ; :fx [[:dispatch [:update-data path]]]
        })
        {:db (update-in db (pop path) dissoc (peek path))}
      )))
  )


(rf/reg-event-db
  :update-new
  (fn [db [_ path val]]
    (assoc-in db path val)))

(rf/reg-event-fx
  :save-new
  (fn [{:keys [db] :as cofx} [_ path redirect]]
    {:db (assoc-in db (conj path :saved) true)
     :fx [
     [:dispatch [:common/navigate! (:url-key redirect) (:params redirect) (:query redirect)]]
     ]
     ; :show-alert (string/join " " (read-string (:params redirect)))
    }))
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
  :path-params
  :<- [:common/route]
  (fn [route] (-> route :path-params)))

(rf/reg-sub
  :query-params
  :<- [:common/route]
  (fn [route] (-> route :query-params)))

(defn remove-opt-key [path-seq]
  (reverse (rest (rest (reverse path-seq)))))

(rf/reg-sub
  :parent-info
  (fn [db [_ path-to-opt]]
    (get-in db (remove-opt-key path-to-opt))))

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
  :skiers
  :<- [:entry-info]
  (fn [info _] (-> info :people :options)))

(rf/reg-sub
  :active-skiers
  :<- [:skiers]
  (fn [skiers]
    (filter #(-> % second :selected) skiers
    )))

(rf/reg-sub
  :skier-info
  :<- [:skiers]
  (fn [skiers [_ skier-id]]
    (skier-id skiers)))

(rf/reg-sub
  :valid-skier-selections?
  :<- [:skiers]
  (fn [skiers [_ skier-id]]
    (all-valid? (-> skiers skier-id :attributes))
    )
)

(rf/reg-sub
  :active-skier-valid-selections?
  :<- [:active-skiers]
  (fn [skiers _]
    (not (some false? (map (fn [[_ v]] (all-valid? (v :attributes))) skiers)))
    )
  )

(rf/reg-sub
  :item-info
  (fn [db [_ path]]
    (get-in db path)))

(rf/reg-sub
  :complete-new-item?
  (fn [db [_ seq-path]]
    (complete-new-item? db seq-path)))

(rf/reg-sub
  :complete-new-type?
  (fn [db [_ seq-path]]
    (complete-new-type? db seq-path)))

(rf/reg-sub
  :common/error
  (fn [db _]
    (:common/error db)))
