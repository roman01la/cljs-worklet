(ns worklet.core
  (:refer-clojure :exclude [fn])
  (:require [cljs.analyzer :as ana]
            [cljs.compiler :as comp]
            [cljs.env :as env]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.walk :as walk])
  (:import (cljs.tagged_literals JSValue)))

(defn preprocess-form
  "Rewrites some Clojure forms

  (-deref x) -> (.-value x)
  (reset! x v) -> (set! (.-value x) v)"
  [form]
  (walk/postwalk
    (clojure.core/fn [x]
      (cond
        (and (list? form) (= 'reset! (first form)))
        (let [[_ target & args] form]
          `(~'set! (.-value ~target) ~@(map preprocess-form args)))

        (and (symbol? x) (= 'clojure.core/deref x))
        '.-value

        (= (type x) JSValue)
        (JSValue. (preprocess-form (.-val x)))

        :else x))
    form))

(defn compile-form
  "Takes env and ClojureScript form and returns compiled JavaScript"
  [env form]
  (let [comp-env (env/default-compiler-env)]
    (swap! comp-env assoc :shadow/ns-roots #{})
    (env/with-compiler-env comp-env
                           (with-out-str
                             (->> form (ana/analyze env) comp/emit)))))

(defn- ast->seq [ast]
  (tree-seq :children (clojure.core/fn [{:keys [children] :as ast}]
                        (let [get-children (apply juxt children)]
                          (->> (get-children ast)
                               (mapcat #(if (vector? %) % [%])))))
            ast))

(defn- find-free-variables [env f]
  (let [ast (ana/analyze env f)]
    (->> (ast->seq ast)
         (filter #(or
                    (contains? #{:js-var :var} (:op %)) ;; should be either js or cljs var
                    (and (= :local (:op %)) ;; or a local
                         (get-in env [:locals (:name %) :name]) ;; from an outer scope
                         (-> % :info :shadow not)))) ;; but not a local shadowing locals from outer scope
         (map :name)
         (into #{}))))

(defn form->worklet-fn-str
  "Takes args vector, function body, env and local vars.
  Returns compiled body as JavaScript string suitable to run as a worklet fn."
  [args body env locals]
  (let [closure `(~'js* ~(str "const { " (str/join ", " (set (map first locals))) " } = jsThis._closure"))]
    (compile-form env
                  `(clojure.core/fn ~args
                     ~closure
                     ~@body))))

(defn env->location-str
  "Takes env and returns location string

  'absolute path (line:column)'"
  [env]
  (let [file-path (-> env :ns :meta :file io/file .getAbsolutePath)]
    (str file-path " (" (:line env) ":" (:column env) ")")))

(defn map->js-obj [m]
  (if-not (map? m)
    m
    (let [kvs-str (->> (keys m)
                       (mapv #(str \' % "':~{}"))
                       (interpose ",")
                       (apply str))]
      `(~'js* ~(str "{" kvs-str "}") ~@(mapv map->js-obj (vals m))))))

;; well known globals
;; that are already available in a worklet thread
(def globals
  '#{this console performance _setGlobalConsole _chronoNow Date Array ArrayBuffer Int8Array Int16Array Int32Array
     Uint8Array Uint8ClampedArray Uint16Array Uint32Array Float32Array Float64Array HermesInternal JSON Math
     Number Object String Symbol undefined null UIManager requestAnimationFrame _WORKLET arguments Boolean parseInt
     parseFloat Map Set _log _updatePropsPaper _updatePropsFabric _removeShadowNodeFromRegistry RegExp Error global
     _measure _scrollTo _dispatchCommand _setGestureState _getCurrentTime _eventTimestamp _frameTimestamp isNaN
     LayoutAnimationRepository _stopObservingProgress _startObservingProgress})

(defn known-global-var? [sym]
  (->> (str/split (name sym) #"\.")
       first
       symbol
       (contains? globals)))

(defn locals->paths [locals]
  (keep (clojure.core/fn [sym]
          (if-let [var-ns (namespace sym)]
            (->> (if (= "js" var-ns)
                   (when-not (known-global-var? sym)
                     (conj (vec (str/split (name sym) #"\.")) sym))
                   (into (vec (str/split var-ns #"\.")) [(name sym) sym])))
            [(str sym) sym]))
        locals))

(defn locals->closure-map [locals]
  (->> locals
       (reduce (clojure.core/fn [ret path]
                 (assoc-in ret (vec (butlast path)) (last path)))
               {})
       map->js-obj))

;;;;;;;;; Public API

(defmacro fn
  "Similar to clojure.core/fn in syntax, but emits a worklet function
  that is meant to be executed on a separate thread in React Native and
  have access to shared values from outer scope.

  (worklet-fn []
    #js {:transform #js [#js {:translateY (.. position -value)}]})"
  [args & body]
  (let [body (map preprocess-form body)
        locals (locals->paths (find-free-variables &env `(clojure.core/fn ~args ~@body)))
        closure (locals->closure-map locals)
        worklet-str (form->worklet-fn-str args body &env locals)
        location (env->location-str &env)]
    `(let [fname# (clojure.core/fn ~args ~@body)]
       (set! (.-_closure fname#) ~closure)
       (set! (.-asString fname#) ~worklet-str)
       (set! (.-__workletHash fname#) ~(hash [args body]))
       (set! (.-__location fname#) ~location)
       fname#)))
