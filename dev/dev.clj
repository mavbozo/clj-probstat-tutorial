(ns dev
  (:require
   [clojure.edn :as edn]
   [scicloj.clay.v2.api :as clay]))

(def clay-config (edn/read-string (slurp "clay.edn")))

(defn make-docs
  [& _]
  (clay/make! (assoc clay-config :show false))
  (shutdown-agents)
  (System/exit 0))
