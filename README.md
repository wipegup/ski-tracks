# ski-tracks

generated using Luminus version "4.44"

FIXME

## Prerequisites

You will need [Leiningen][1] 2.0 or above installed.

[1]: https://github.com/technomancy/leiningen

## Running

To start a web server for the application, run:

    lein run

## License

Copyright © 2022 FIXME


```
npx shadow-cljs watch app
lein run
lein repl :connect localhost:7000

#Start Local DDB
java -Djava.library.path=./DynamoDBLocal_lib -jar DynamoDBLocal.jar -sharedDb
```

## DynamoDB Organization
table: ski-tracks
pk: string - top level name e.g. "observation" "item-types" "context"
rk: <prefix?>-timestamp
data: <map>

```clj
;;; Dev setup
(require '[taoensso.faraday :as far])
(require '[java-time :as jt])
(require '[clojure.data.json :as json])
(def ddb-opts
  {:access-key "fake-access"
   :secret-key "fake-secret"
   :endpoint "http://localhost:8000"
  })

(defn ts [] (jt/format "yyyy-MM-dd'T'HH:mm:ss.SS" (jt/local-date-time)))
(def test-table :test-ski-tracks)
(far/create-table ddb-opts test-table
  [:pk :s]
  {:range-keydef [:rk :s]
   :throughput {:read 10 :write 10} ; Read & write capacity (units/sec)
   :block? true ; Block thread during table creation
   })

(def js
  (-> "resources/public/json/example.json"
   slurp
   (json/read-str :key-fn keyword)))

(defn put-item [pk rk data]
  (far/put-item ddb-opts test-table {:pk pk :rk rk :data data}))

;; Add context to ddb
(let [pk "context"
      rk (ts)
      data (dissoc js :people :resort)]
  (put-item pk rk data))

;; Add people to ddb
(let [pk "items"
      rk (str "people-" (ts))
      data (js :people)]
      (put-item pk rk data))

;; Add resort to ddb
(let [pk "items"
      rk (str "resort-" (ts))
      data (js :resort)]
  (put-item pk rk data))

;; e.g retrieve latest context
;; In lieu of maintaining a "latest" value which tells the specific RK to query
;; Not too worried about read/write costs
; (far/query ddb-opts test-table {:pk [:eq "context"]} {:return [:rk :data] :order :desc :limit 1})
```


## TODO
- Add temperature
- Add weather
- Add departure time
- Add parking lot arrival time
- Add parking lot status
- Seed Database

- Save new info to DB
- Save observations (add submit button)

- Expand hiking input (v2)
- Display previous observations
