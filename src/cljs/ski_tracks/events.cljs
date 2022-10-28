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
  :set-entry-info
  (fn [db [_ info]]
    (assoc db :entry-info info)))

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

(def run-blank {:runs {} :comment "" :run-num 0 :hike-vert-mod "0"})

(rf/reg-event-db
  :page/init-run-select
  (fn [db _]
    (when (-> db :run-info not) (assoc db :run-info run-blank ))))


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

(defn attributes-path [path]
  (extend-path path :options))

(defn selected-option-path [path key]
  (extend-path (options-path path) key))

(defn selected-attributes-path [path key]
  (extend-path (attributes-path path) key))

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
          )])
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
  (vec (map #(keyword %) (string/split path-string #"/"))))

(def option-blank
  {:name "" :description "" :selected false :attributes {}})

(def attribute-blank
  {:type "" :required true :name "" :options {} :valid? false})

(def resort-blank (merge option-blank {:attributes {:hike {:name "Hike" :options {}} :lift {:name "Lift" :options {}} :ski {:name "Ski" :options {}}}}))

(defn get-new-blank [kind]
  (condp = kind
    :option option-blank
    :attribute attribute-blank
    :resort resort-blank))

(rf/reg-event-db
  :add-new-blank-opt
  (fn [db [_ type]]
    (let [
      path (get-keyword-path type)
      kind (if (get-in db (conj path :attributes)) :attribute :option)]
    (assoc-in db [:new-entry] (merge
      (get-new-blank (if (= (last path) :resort) :resort kind))
      {:path path :kind kind})))))

(defn need-vert? [path-seq]
  (some #{:hike :lift} path-seq))

(rf/reg-event-db
  :discard-new
  (fn [db _] (dissoc db :new-entry)))


(rf/reg-event-db
  :update-new
  (fn [db [_ k val]]
    (assoc-in db [:new-entry k] val)))

(rf/reg-event-fx
  :save-new
  (fn [{:keys [db] :as cofx} [_ path redirect]]
    (let [entry (dissoc (:new-entry db) :path :kind)
          tog (if (contains? entry :required) (assoc entry :valid? (-> entry :required not)) entry)
          ]
    {:db (assoc-in db path tog)
     :fx [
     [:dispatch [:common/navigate! (:url-key redirect) (:params redirect) (:query redirect)]]
     ]
     ; :show-alert (string/join " " (read-string (:params redirect)))
    })))

(rf/reg-event-db
  :add-run
  (fn [db [_ m]]
    (let [
      num-path [:run-info :run-num]
      mod-path [:run-info :hike-vert-mod]
      path  [:run-info :runs (get-in db num-path)]
      added (if (= (:type m) :hike)
        (assoc-in db path (assoc m :vert (+ (js/parseInt (get-in db mod-path))(:vert m))))
        (assoc-in db path  m)
      )
      clear-comment (assoc-in added [:run-info :comment] "")
      clear-hike-mod (assoc-in clear-comment [:run-info :hike-vert-mod] "0")
      ]
      (assoc-in clear-hike-mod num-path (inc (get-in clear-hike-mod num-path)))
    )
    )
  )

(rf/reg-event-db
  :update-run-info
  (fn [db [_ k v]]
    (let [path  [:run-info k]]
      (assoc-in db path v))))

(rf/reg-event-db
  :delete-last-run
  (fn [db _]
    (let [path  [:run-info :runs]
        num-path  [:run-info :run-num]
      del (update-in db path dissoc (apply max (keys (get-in db path))))]
      (assoc-in del num-path (dec (get-in del num-path)))
      )
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
  :path-params
  :<- [:common/route]
  (fn [route] (-> route :path-params)))

(rf/reg-sub
  :get-path-param
  :<- [:common/route]
  (fn [route [_ k]] (-> route :path-params k)))

(rf/reg-sub
  :query-params
  :<- [:common/route]
  (fn [route] (-> route :query-params)))

(rf/reg-sub
  :common/error
  (fn [db _]
    (:common/error db)))

(rf/reg-sub
  :new-entry
  (fn [db _] (:new-entry db)))

(rf/reg-sub
  :entry-info
  (fn [db _] (:entry-info db)))

(rf/reg-sub
  :active-resort-id
  :<- [:entry-info]
  (fn [info]
    (ffirst (filter #(-> % second :selected) (-> info :resort :options)))))

(rf/reg-sub
  :resort-runs
  :<- [:entry-info]
  :<- [:active-resort-id]
  (fn [[info id] _]
    (-> info :resort :options id :attributes)))

(rf/reg-sub
  :run-info
  (fn [db _] (:run-info db)))

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
  :item-info
  (fn [db [_ path]]
    (get-in db path)))
