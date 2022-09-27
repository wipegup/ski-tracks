(ns ski-tracks.env
  (:require
    [selmer.parser :as parser]
    [clojure.tools.logging :as log]
    [ski-tracks.dev-middleware :refer [wrap-dev]]))

(def defaults
  {:init
   (fn []
     (parser/cache-off!)
     (log/info "\n-=[ski-tracks started successfully using the development profile]=-"))
   :stop
   (fn []
     (log/info "\n-=[ski-tracks has shut down successfully]=-"))
   :middleware wrap-dev})
