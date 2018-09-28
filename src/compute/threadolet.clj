(ns compute.threadolet)

(defmacro ^{:private true} assert-args
  [& pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                  (str (first ~'&form) " requires " ~(second pairs) " in " ~'*ns* ":" (:line (meta ~'&form))))))
       ~(let [more (nnext pairs)]
          (when more
            (list* `assert-args more)))))

(defn- let-template*
  [context-fn bindings body prev-binding-form prev-value-sym]
  (let [form (if (empty? bindings)
               `(do
                  ~@body)
               (let [[[binding-form binding-expr] & bindings] bindings
                     value-sym (gensym "value")]
                 `(let [~value-sym ~binding-expr]
                    ~(context-fn value-sym (partial let-template* context-fn bindings body binding-form value-sym)))))]
    (if prev-value-sym
      `(let [~prev-binding-form ~prev-value-sym]
         ~form)
      form)))

(defn let-template
  [context-fn bindings body]
  #_(assert-args
      (vector? bindings) "a vector for its binding"
      (even? (count bindings)) "an even number of forms in binding vector")
  (let-template* context-fn (partition 2 (destructure bindings)) body nil nil))

(defn as->template
  [context-fn x forms value-sym]
  (if forms
    (let [form (first forms)]
      `(let [~value-sym ~x]
         ~(context-fn value-sym (partial as->template context-fn form (next forms) value-sym))))
    `(let [~value-sym ~x]
       ~(context-fn value-sym (constantly value-sym)))))

(defn ->template
  [context-fn x forms thread-fn]
  (let [value-sym (gensym "value")]
    (as->template context-fn x (thread-fn value-sym forms) value-sym)))

(defn thread-first
  [value-sym forms]
  (map #(if (seq? %)
          (with-meta `(~(first %) ~value-sym ~@(next %)) (meta %))
          (list % value-sym))
       forms))

(defn thread-last
  [value-sym forms]
  (map #(if (seq? %)
          (with-meta `(~(first %) ~@(next %) ~value-sym) (meta %))
          (list % value-sym))
       forms))

(defn short-circuit
  [pred value-sym recur-fn]
  `(if (~pred ~value-sym)
     ~value-sym
     ~(recur-fn)))