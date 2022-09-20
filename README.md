# cljs-worklet

_Run ClojureScript functions on a worklet thread in React Native_

[About worklets](https://docs.swmansion.com/react-native-reanimated/docs/2.2.x/worklets/)

## Usage
```clojure
(ns app.core
  (:require [worklet.core :as w]
            [uix.core :refer [defui $]]
            [react-native-reanimated :as rnr :default Animated]
            [react-native-gesture-handler :as rngh]))

(defui animated-view []
  (let [position (rnr/useSharedValue 0) ;; creates a shared value
       animated-styles (rnr/useAnimatedStyle
                         ;; generates workletized animation function
                         (w/fn []
                           #js {:transform #js [#js {:translateY @position}]}))
       gesture (-> (.Fling rngh/Gesture)
                   (.direction (.-DOWN rngh/Directions))
                   (.onStart
                     ;; generates workletized gesture handler function
                     (w/fn []
                       (reset! position
                               (rnr/withSpring
                                 (+ 100 @position)
                                 #js {:damping 10
                                      :mass 0.3})))))]
   ($ rngh/GestureDetector {:gesture gesture}
     ($ (.-View Animated) {:style animated-styles}
        ...))))
```

## How it works

`worklet.core/fn` macro emits a workletized anonoymous function that is ready to be run on React Native's UI thread.
None of ClojureScript standard library is available in native UI thread, calling something like `(map inc [1 2 3])` is not gonna work.

However, it's still possible to execute things in JS thread from native UI thread via `runOnJS` function, [read more about it here](https://docs.swmansion.com/react-native-reanimated/docs/api/miscellaneous/runOnJS/).

The purpose of `worklet.core/fn` macro is to create animation running and user input handling functions that will execute on native UI thread, which is where animations are running and user input is received, so that both animations and input handling is not interrupted by application code running on JS thread.
