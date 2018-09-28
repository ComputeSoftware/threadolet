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

(defn ->template
  [context-fn x forms]
  (let [value-sym (gensym "value")]
    (if forms
      (let [form (first forms)
            x' (if (seq? form)
                 (with-meta `(~(first form) ~value-sym ~@(next form)) (meta form))
                 (list form value-sym))]
        `(let [~value-sym ~x]
           ~(context-fn value-sym (partial ->template context-fn x' (next forms)))))
      `(let [~value-sym ~x]
         ~(context-fn value-sym (constantly value-sym))))))

(defn ->>template
  [context-fn x forms]
  (let [value-sym (gensym "value")]
    (if forms
      (let [form (first forms)
            x' (if (seq? form)
                 (with-meta `(~(first form) ~@(next form) ~value-sym) (meta form))
                 (list form value-sym))]
        `(let [~value-sym ~x]
           ~(context-fn value-sym (partial ->>template context-fn x' (next forms)))))
      `(let [~value-sym ~x]
         ~(context-fn value-sym (constantly value-sym))))))

(defn as->template
  [context-fn x name forms]
  (if forms
    (let [form (first forms)]
      `(let [~name ~x]
         ~(context-fn name (partial as->template context-fn form name (next forms)))))
    `(let [~name ~x]
       ~(context-fn name (constantly name)))))
