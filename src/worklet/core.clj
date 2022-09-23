(ns worklet.core
  (:refer-clojure :exclude [fn defn future])
  (:require [clojure.core :as core]
            [cljs.analyzer :as ana]
            [cljs.compiler :as comp]
            [cljs.env :as env]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.walk :as walk])
  (:import (cljs.tagged_literals JSValue)))

(core/defn preprocess-form [env form]
  (walk/postwalk
    (clojure.core/fn [x]
      (or
        (when (symbol? x)
          (let [v (ana/resolve-var env x)]
            (when (= 'cljs.core (:ns v))
              (let [s (symbol (str "worklet.core/" (name (:name v))))
                    v (ana/resolve-var env s)]
                (when (:tag v)
                  s)))))
        (cond
          (and (list? x) (= 'reset! (first x)))
          (let [[_ target & args] x]
            `(~'set! (.-value ~target) ~@(map (partial preprocess-form env) args)))

          (and (symbol? x) (= 'clojure.core/deref x))
          '.-value

          (= (type x) JSValue)
          (JSValue. (preprocess-form env (.-val x)))

          :else x)))
    form))

(core/defn compile-form
  "Takes env and ClojureScript form and returns compiled JavaScript"
  [env form]
  (let [env (assoc env :def-emits-var false)]
    (env/with-compiler-env env/*compiler*
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
                    (contains? #{:js-var :var} (:op %))
                    (and (= :local (:op %)) ;; should be a local
                         (get-in env [:locals (:name %) :name]) ;; from an outer scope
                         (-> % :info :shadow not)))) ;; but not a local shadowing locals from outer scope
         (map :name)
         (into #{}))))

(core/defn form->worklet-fn-str
  "Takes args vector, function body, env and local vars.
     Returns compiled body as JavaScript string suitable to run as a worklet fn."
  [fn-name args body env locals]
  (let [closure `(~'js* ~(str "const { " (str/join ", " (set (map first locals))) " } = jsThis._closure"))]
    (compile-form env
                  `(clojure.core/fn ~fn-name ~args
                     ~closure
                     ~@body))))

(core/defn env->location-str
  "Takes env and returns location string

     'absolute path (line:column)'"
  [env]
  (let [file-path (some-> env :ns :meta :file io/file .getAbsolutePath)]
    (str (or file-path "<unknown>")
         " (" (:line env) ":" (:column env) ")")))

(core/defn map->js-obj [m]
  (if-not (map? m)
    m
    (let [kvs-str (->> (keys m)
                       (mapv #(str \' % "':~{}"))
                       (interpose ",")
                       (apply str))]
      `(~'js* ~(str "{" kvs-str "}") ~@(mapv map->js-obj (vals m))))))

(def globals
  '#{this console performance _setGlobalConsole _chronoNow Date Array ArrayBuffer Int8Array Int16Array Int32Array
     Uint8Array Uint8ClampedArray Uint16Array Uint32Array Float32Array Float64Array HermesInternal JSON Math
     Number Object String Symbol undefined null UIManager requestAnimationFrame _WORKLET arguments Boolean parseInt
     parseFloat Map Set _log _updatePropsPaper _updatePropsFabric _removeShadowNodeFromRegistry RegExp Error global
     _measure _scrollTo _dispatchCommand _setGestureState _getCurrentTime _eventTimestamp _frameTimestamp isNaN
     LayoutAnimationRepository _stopObservingProgress _startObservingProgress})

(core/defn known-global-var? [sym]
  (->> (str/split (name sym) #"\.")
       first
       symbol
       (contains? globals)))

(core/defn locals->paths [locals]
  (keep (clojure.core/fn [sym]
          (if-let [var-ns (namespace sym)]
            (->> (if (= "js" var-ns)
                   (when-not (known-global-var? sym)
                     (conj (vec (str/split (name sym) #"\.")) sym))
                   (into (vec (str/split var-ns #"\.")) [(name sym) sym])))
            [(str sym) sym]))
        locals))

(core/defn locals->closure-map [locals]
  (->> locals
       (reduce (clojure.core/fn [ret path]
                 (assoc-in ret (vec (butlast path)) (last path)))
               {})
       map->js-obj))

(defmacro fn
  "Similar to clojure.core/fn in syntax, but emits a worklet function
  that is meant to be executed on a separate thread in React Native and
  have access to shared values from outer scope.

  (worklet-fn []
    #js {:transform #js [#js {:translateY (.. position -value)}]})"
  [fname args & body]
  (let [fn-name (if (symbol? fname)
                  fname
                  (symbol (gensym "wfn")))
        [args body] (cond
                      (symbol? fname) [args body]
                      (vector? fname) [fname (into [args] body)])
        body (map (partial preprocess-form &env) body)
        locals (locals->paths (find-free-variables &env `(clojure.core/fn ~fn-name ~args ~@body)))
        closure (locals->closure-map locals)
        worklet-str (form->worklet-fn-str fn-name args body &env locals)
        location (env->location-str &env)]
    `(let [fname# (clojure.core/fn ~fn-name ~args ~@body)]
       (set! (.-_closure fname#) ~closure)
       (set! (.-asString fname#) ~worklet-str)
       (set! (.-__workletHash fname#) ~(hash [args body]))
       (set! (.-__location fname#) ~location)
       fname#)))

(defmacro defn
  "Similar to clojure.core/defn in syntax, but emits a worklet function
  that is meant to be executed on a separate thread in React Native and
  have access to shared values from outer scope.

  (w/defn []
    #js {:transform #js [#js {:translateY (.. position -value)}]})"
  [fname args & body]
  `(def ~fname (fn ~fname ~args ~@body)))
