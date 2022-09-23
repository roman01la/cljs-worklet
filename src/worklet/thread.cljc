(ns worklet.thread
  #?(:clj (:require [worklet.core]))
  #?(:cljs (:require-macros [worklet.thread]))
  #?(:cljs (:require [react-native-multithreading :as rnm])))

#?(:cljs
   (defn spawn-thread [f]
     (rnm/spawnThread f)))

#?(:clj
   (defmacro future [& body]
     `(spawn-thread (worklet.core/fn [] ~@body))))
