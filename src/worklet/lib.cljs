(ns worklet.lib
  (:refer-clojure :exclude [map filter reduce])
  (:require [worklet.core :as w]))

(w/defn map [f coll]
  (.map coll f))

(w/defn filter [f coll]
  (.filter coll f))

(w/defn reduce [f init coll]
  (.reduce coll f init))
