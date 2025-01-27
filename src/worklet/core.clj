(ns worklet.core
  (:refer-clojure :exclude [fn defn])
  (:require [cljs.source-map :as sm]
            [cljs.vendor.clojure.data.json :as json]
            [clojure.core :as core]
            [cljs.analyzer :as ana]
            [cljs.compiler :as comp]
            [cljs.env :as env]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk])
  (:import (cljs.tagged_literals JSValue)
           (java.io File)
           (java.util.concurrent.atomic AtomicLong)))

(def ^:private goog-debug (with-meta 'goog.DEBUG {:tag 'boolean}))

(core/defn preprocess-form
  "Rewrites get/set syntax for shared values (useSharedValue)
  @ref -> ref.value
  (reset! ref val) -> (set! (.-value ref) val)"
  [env form]
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

(defn- url-path [^String f]
  (.getPath (.toURL (.toURI (io/file f)))))

(core/defn compile-form
  "Takes env and ClojureScript form and returns compiled JavaScript"
  [env form]
  (binding [comp/*source-map-data* (atom {:source-map (sorted-map) :gen-line 0})
            comp/*source-map-data-gen-col* (AtomicLong.)]
    (let [env (assoc env :def-emits-var false)
          js-str (env/with-compiler-env env/*compiler*
                   (with-out-str
                     (->> form (ana/analyze env) comp/emit)))
          sm-data (assoc @comp/*source-map-data* :gen-col (.get ^AtomicLong comp/*source-map-data-gen-col*))
          sm-json (-> (sm/encode* {(url-path ana/*cljs-file*) (:source-map sm-data)}
                                  {:lines (+ (:gen-line sm-data) 2)
                                   :file (url-path ana/*cljs-file*)})
                      (dissoc "file")
                      json/write-str)]
      [js-str sm-json])))

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
  (let [closure `(~'js* ~(str "const { " (str/join ", " (set (map first locals))) " } = this.__closure"))]
    (compile-form env
                  `(clojure.core/fn ~fn-name ~args
                     ~closure
                     ~@body))))

(core/defn map->js-obj [m]
  (if-not (map? m)
    m
    (let [kvs-str (->> (keys m)
                       (mapv #(str \' % "':~{}"))
                       (interpose ",")
                       (apply str))]
      `(~'js* ~(str "{" kvs-str "}") ~@(mapv map->js-obj (vals m))))))

(core/defn vec->js-array [s]
  (if-not (vector? s)
    s
    (let [vals-str (->> s
                        (mapv (constantly "~{}"))
                        (interpose ",")
                        (apply str))]
      `(~'js* ~(str "[" vals-str "]") ~@s))))

;; https://github.com/software-mansion/react-native-reanimated/blob/main/packages/react-native-reanimated/plugin/src/globals.ts
(def globals
  '#{globalThis Infinity NaN undefined eval isFinite isNaN parseFloat parseInt decodeURI decodeURIComponent encodeURI
     encodeURIComponent escape unescape Object Function Boolean Symbol Error AggregateError EvalError RangeError
     ReferenceError SyntaxError TypeError URIError InternalError Number BigInt Math Date String RegExp Array Int8Array
     Uint8Array Uint8ClampedArray Int16Array Uint16Array Int32Array Uint32Array BigInt64Array BigUint64Array
     Float32Array Float64Array Map Set WeakMap WeakSet ArrayBuffer SharedArrayBuffer DataView Atomics JSON WeakRef
     FinalizationRegistry Iterator AsyncIterator Promise GeneratorFunction AsyncGeneratorFunction Generator
     AsyncGenerator AsyncFunction Reflect Proxy Intl null this global window console performance
     queueMicrotask requestAnimationFrame setImmediate arguments HermesInternal _WORKLET ReanimatedError
     __reanimatedLoggerConfig})

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
    #js {:transform #js [#js {:translateY @position}]})"
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
        [worklet-str sm-json] (form->worklet-fn-str fn-name args body &env locals)
        sm-json `(when ~goog-debug ~sm-json)
        location `(when ~goog-debug ~ana/*cljs-file*)]
    `(let [fname# (clojure.core/fn ~fn-name ~args ~@body)]
       (set! (.-__closure fname#) ~closure)
       (set! (.-__initData fname#) ~(map->js-obj {"code" worklet-str
                                                  "location" location
                                                  "sourceMap" sm-json}))
       (set! (.-__workletHash fname#) ~(hash [args body]))
       (set! (.-__stackDetails fname#) ~(vec->js-array '[(js/global.Error.) -2 -27]))
       fname#)))

(defmacro defn
  "Similar to clojure.core/defn in syntax, but emits a worklet function
  that is meant to be executed on a separate thread in React Native and
  have access to shared values from outer scope.

  (w/defn []
    #js {:transform #js [#js {:translateY @position}]})"
  [fname args & body]
  `(def ~fname (fn ~fname ~args ~@body)))
