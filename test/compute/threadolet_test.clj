(ns compute.threadolet-test
  (:require [clojure.test :refer :all]
            [compute.threadolet :refer :all]))

(defn short-circuit
  [pred value-sym recur-fn]
  `(if (~pred ~value-sym)
     ~value-sym
     ~(recur-fn)))

(defmacro alet
  [bindings & body]
  (let-template (partial short-circuit nil?) bindings body))

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
  (->template (partial short-circuit nil?) x forms thread-first))

(deftest short-circuit-nil-thread-first
  (is (= -3 (a-> 1
                 inc
                 (- 5))))

  (is (nil? (a-> 1
                 inc
                 ((constantly nil))
                 (- 5)))))




(defmacro a-as->
  [x name & forms]
  (as->template (partial short-circuit nil?) x forms name))

(deftest short-circuit-nil-as-first
  (is (= 3 (a-as-> 1 z
                   (inc z)
                   (- 5 z))))

  (is (nil? (a-as-> 1 z
                    (inc z)
                    ((constantly nil) z)
                    (- 5 z)))))

(defmacro e->>
  [x & forms]
  (->template (partial short-circuit #(and (seqable? %) (empty? %))) x forms thread-last))

(deftest short-circuit-empty-thread-last
  (is (= false (e->> [2 4]
                     (map inc)
                     (filter odd?)
                     empty?)))

  (is (empty? (e->> [2 4]
                    (map inc)
                    (filter even?)
                    empty?))))

(defn replace-nil
  [value-sym recur-fn]
  `(if (nil? ~value-sym)
     (let [~value-sym 0]
       ~(recur-fn))
     ~(recur-fn)))

(defmacro r->
  [x & forms]
  (->template replace-nil x forms thread-first))

(deftest replace-nil-thread-first
  (is (= -3 (r-> 1
                 inc
                 (- 5))))

  (is (= -5 (r-> 1
                 inc
                 ((constantly nil))
                 (- 5)))))

(defn log
  [value-sym recur-fn]
  `(do
     (println ~(name value-sym) ~value-sym)
     ~(recur-fn)))

(defmacro log-as->
  [x name & forms]
  (as->template log x forms name))

(comment
  (log-as-> 1 z
          (inc z)
          (- 5 z))

  (log-as-> 1 z
          (inc z)
          ((constantly nil) z)
          (- 5 z)))