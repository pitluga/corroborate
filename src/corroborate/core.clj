(ns corroborate.core
  (:use [clojure.string :only [blank?]]))

(defn- validate-field [topic field validations errors]
  (let [field-errors (remove nil? (map #(% topic field) (flatten [validations])))]
    (if (empty? field-errors) errors (conj errors [field field-errors]))))

(defn validate [topic & fields-with-validations]
  (loop [[field validations & more] fields-with-validations errors {}]
    (if (nil? field)
      errors
      (recur more (validate-field topic field validations errors)))))

(defmacro defvalidator [name & fields-with-validations]
  `(defn ~name [topic#]
     (validate topic# ~@fields-with-validations)))

(defn validate-staged [model & validators]
  (loop [[validator & more] validators]
    (if (nil? validator)
      {}
      (let [errors (validator model)]
        (if (empty? errors) (recur more) errors)))))

(defn is-required
  ([] (is-required "is required"))
  ([message]
    #(let [value (%2 %1)]
      (cond
        (string? value) (if (blank? value) message)
        (coll? value) (if (empty? value) message)
        :else (if (nil? value) message)))))

(defn is [predicate message]
  #(if-not (predicate (%2 %1)) message))

(defn is-not [predicate message]
  #(if (predicate (%2 %1)) message))

(defn is-formatted
  ([pattern] (is-formatted pattern "is improperly formatted"))
  ([pattern message] (is #(re-matches pattern (str %1)) message)))

(defn is-included-in
  ([accepted-values] (is-included-in accepted-values "is not included in the list"))
  ([accepted-values message] (is (partial contains? accepted-values) message)))

(defn is-excluded-from
  ([excluded-values] (is-excluded-from excluded-values "is reserved"))
  ([excluded-values message] (is-not (partial contains? excluded-values) message)))

(defn is-confirmed-by
  ([field] (is-confirmed-by field "does not match"))
  ([field message] #(if-not (= (%2 %1) (field %1)) message)))

(defn is-numeric
  ([] (is-numeric "is not a number"))
  ([message] (is number? message)))

(defmacro only-if [pred validation]
  `(fn [topic# field#]
    (if (~pred topic# field#)
      (~validation topic# field#))))

(defmacro only-if-not [pred validation]
  `(fn [topic# field#]
    (if-not (~pred topic# field#)
      (~validation topic# field#))))

(defn allow-nil [validation]
  (only-if-not #(nil? (%2 %1)) validation))
