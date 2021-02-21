(ns user
  (:require
   [clojure.core.matrix :as m]
   [org.suskalo.geom.ops :as ops]))

(defonce
  ^{:doc "Runs some code once when the application loads."}
  on-load
  (do
    (m/set-current-implementation :vectorz)))
