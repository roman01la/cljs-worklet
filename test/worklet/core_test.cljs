(ns worklet.core-test
  (:require [cljs.test :refer [deftest is run-tests]]
            [worklet.core :as w]))

(let [x 1
      v #js {:value 3}]
  (w/defn test-fn []
    (reset! v (+ x @v))))

(deftest test-defn
  (is (= 4 (test-fn)))
  (is (= 1 (.. test-fn -__closure -x)))
  (is (= -1068664973 (.. test-fn -__workletHash)))
  (let [[e c l] (.. test-fn -__stackDetails)]
    (is (array? (.. test-fn -__stackDetails)))
    (is (instance? js/Error e))
    (is (= -2 c))
    (is (= -27 l)))
  (is (= (.. test-fn -__initData -code)
         "(function worklet$core_test$test_fn(){\nconst { x, v } = this.__closure;\n\nreturn (v.value = (x + v.value));\n})"))
  (is (= (.. test-fn -__initData -location)
         "core_test.cljs"))
  (is (= (.. test-fn -__initData -sourceMap)
         "{\"version\":3,\"sources\":[\"core_test.cljs\"],\"lineCount\":6,\"mappings\":\"AAME;AAAA,AAAA;;AAAA,QAAA,AACUA,UAAE,KAAA,JAAGC,IAAGD\",\"names\":[\"v\",\"x\"]}")))

(defn -main [& args]
  (let [{:keys [fail error]} (run-tests
                               'worklet.core-test)]
    (js/console.assert (zero? (+ fail error)) "Tests failed")))