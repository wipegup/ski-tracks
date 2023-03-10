(ns ski-tracks.routes.services
  (:require
    [reitit.swagger :as swagger]
    [reitit.swagger-ui :as swagger-ui]
    [reitit.ring.coercion :as coercion]
    [reitit.coercion.spec :as spec-coercion]
    [reitit.ring.middleware.muuntaja :as muuntaja]
    [reitit.ring.middleware.multipart :as multipart]
    [reitit.ring.middleware.parameters :as parameters]
    [ski-tracks.middleware.formats :as formats]
    [ring.util.http-response :refer :all]
    [clojure.java.io :as io]
    [clojure.data.json :as json]
    [taoensso.faraday :as far]
    [java-time :as jt]
    [clojure.string :as string]))

    (defn add-to-arr [h mod-func]
      (into {}
        (map
          (fn [[k v]] [k (mod-func v)])
          h))
    )

    (defn loop-hashes
      ([h mod-func] (loop-hashes h mod-func {}))
      ([arrs mod-func to-ret]
        (if (empty? arrs)
          to-ret
          (let [[k v] (first arrs)]
            (recur (rest arrs) mod-func (assoc to-ret k (mod-func v)))))))

    (defn add-valid-func [v]
      (assoc v :valid?
        (not
        (:required v)
        )
        ))

    (defn add-selected-func [v]
      (if (:options v)
        (assoc v :options (add-to-arr (:options v) (fn [v] (assoc v :selected false))))
        v))

    (defn inner-prepare-func [v]
      (if (v :options)
        (assoc v :options
          (add-to-arr (:options v)
            (fn [v]
              (if (:attributes v)
                (assoc v :attributes (-> (:attributes v) (loop-hashes add-valid-func) (loop-hashes add-selected-func)))
                v))
              ))
        v))

  (defn prepare-source [h]
    (-> h
      (loop-hashes add-valid-func)
      (loop-hashes add-selected-func)
      (loop-hashes inner-prepare-func)))

(def ddb-opts
  {:access-key "fake-access"
   :secret-key "fake-secret"
   :endpoint "http://localhost:8000"
  })
(def ddb-table :test-ski-tracks)

(defn latest-ddb
  ([pk] (->
    (far/query ddb-opts ddb-table
      {:pk [:eq pk]} {:return [:data] :order :desc :limit 1})
      (get 0)
      :data
      ))
  ([pk rk-prefix] (->
    (far/query ddb-opts ddb-table
      {:pk [:eq pk] :rk [:begins-with (str rk-prefix "-")]} {:return [:data] :order :desc :limit 1})
      (get 0)
      :data
      )))

(defn ts [] (jt/format "yyyy-MM-dd'T'HH:mm:ss.SS" (jt/local-date-time)))

(defn put-item [pk rk data]
  (let [put-result (try
    (far/put-item ddb-opts ddb-table {:pk pk :rk rk :data data})
    (catch Exception e false)
    )]
    (nil? put-result)
    ))

(defn put-generic[pk rk data]
  (let [put-result (try
    (far/put-item ddb-opts ddb-table {:pk pk :rk rk :data data})
    (catch Exception e false)
    )]
    (nil? put-result)
    ))


(defn service-routes []
  ["/api"
   {:coercion spec-coercion/coercion
    :muuntaja formats/instance
    :swagger {:id ::api}
    :middleware [;; query-params & form-params
                 parameters/parameters-middleware
                 ;; content-negotiation
                 muuntaja/format-negotiate-middleware
                 ;; encoding response body
                 muuntaja/format-response-middleware
                 ;; exception handling
                 coercion/coerce-exceptions-middleware
                 ;; decoding request body
                 muuntaja/format-request-middleware
                 ;; coercing response bodys
                 coercion/coerce-response-middleware
                 ;; coercing request parameters
                 coercion/coerce-request-middleware
                 ;; multipart
                 multipart/multipart-middleware]}

   ;; swagger documentation
   ["" {:no-doc true
        :swagger {:info {:title "my-api"
                         :description "https://cljdoc.org/d/metosin/reitit"}}}

    ["/swagger.json"
     {:get (swagger/create-swagger-handler)}]

    ["/api-docs/*"
     {:get (swagger-ui/create-swagger-ui-handler
             {:url "/api/swagger.json"
              :config {:validator-url nil}})}]]

   ["/ping"
    {:get (constantly (ok {:message "pong"}))}]

   ; ["/entry-info-old"
   ;  {:get { :summary "emits static example json"
   ;          :handler (fn [_]
   ;            {:status 200
   ;             :body
   ;               (-> "public/json/example.json"
   ;               io/resource
   ;               slurp
   ;               (json/read-str :key-fn keyword)
   ;               prepare-source )
   ;               })}}]

   ["/entry-info"
    {:get { :summary "emits static example from ddb"
            :handler (fn [_]
              {:status 200
               :body
                 (-> (merge
                   (latest-ddb "context")
                   {:people (latest-ddb "items" "people")}
                   {:resort (latest-ddb "items" "resort")})
                 prepare-source )
                 })}}]

   ["/item-info-update"
    {:post { :summary "Updates ddb with new info"
            :handler (fn [{{:keys [k d path]} :body-params}]

              (let [
                pk "items"
                path (map keyword path)
                rk (str k "-" (ts))

                old (latest-ddb pk k)
                updated (assoc-in old path d)
                put (put-item pk rk updated)
              ]
              {
                :status (if put 200 400)
                :body {:status (if put "OK" "NOT SAVED")}
                }))}}]
   ["/add-obs"
    {:post { :summary "Adds obs to ddb"
            :handler (fn [{{:keys [d]} :body-params}]

              (let [
                pk "obs"
                rk (str (ts))
                ctx (dissoc (d :entry-info) :people :resort)
                p (-> d :entry-info :people)
                p-list  (into {} (filter #( (second %) :selected) (p :options) ))
                pl-attr (m)
                resort (-> d :entry-info :resort)

                ri (d :run-info)
                put (put-item pk rk clean)
              ]
              {
                :status (if put 200 400)
                :body {:status (if put "OK" "NOT SAVED")}
                }))}}]
                 ])
