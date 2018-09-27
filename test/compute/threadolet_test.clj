(ns compute.threadolet-test
  (:require [clojure.test :refer :all]
            [compute.threadolet :refer :all]))

(defn short-circuit-nil
  [value-sym recur-fn]
  `(if (nil? ~value-sym)
     ~value-sym
     ~(recur-fn)))

(defmacro alet
  [bindings & body]
  (let-template short-circuit-nil bindings body))

(alet [x (inc 1)
       y (dec x)]
  (println x y)
  (- x y))

(alet [x (inc 1)
       y nil]
  (println x y)
  (- x y))

(defmacro a->
  [x & forms]
  (->template short-circuit-nil x forms))

(a-> 1
     inc
     (- 5))

(a-> 1
     inc
     ((constantly nil))
     (- 5))