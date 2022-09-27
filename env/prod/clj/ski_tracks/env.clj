(ns ski-tracks.env
  (:require [clojure.tools.logging :as log]))

(def defaults
  {:init
   (fn []
     (log/info "\n-=[ski-tracks started successfully]=-"))
   :stop
   (fn []
     (log/info "\n-=[ski-tracks has shut down successfully]=-"))
   :middleware identity})
