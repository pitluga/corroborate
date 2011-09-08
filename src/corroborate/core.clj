(ns corroborate.core
  (:use [clojure.contrib.string :only [blank?]]))

(defn- validate-field [topic field validations errors]
  (let [field-errors (remove nil? (map #(% topic field) (flatten [validations])))]
    (if (empty? field-errors) errors (conj errors [field field-errors]))))

(defn validate [topic & fields-with-validations]
  (loop [[field validations & more] fields-with-validations errors {}]
    (if (nil? field)
      errors
      (recur more (validate-field topic field validations errors)))))

(defn is-required
  ([] (is-required "is required"))
  ([message]
    #(let [value (%2 %1)]
      (cond
        (string? value) (if (blank? value) message)
        (coll? value) (if (empty? value) message)
        :else (if (nil? value) message)))))

(defn is-formatted
  ([pattern] (is-formatted pattern "is improperly formatted"))
  ([pattern message] #(if-not (re-matches pattern (%2 %1)) message)))

(defn is-included-in
  ([accepted-values] (is-included-in accepted-values "is not included in the list"))
  ([accepted-values message] #(if-not (contains? accepted-values (%2 %1)) message)))

(defn is-excluded-from
  ([excluded-values] (is-excluded-from excluded-values "is reserved"))
  ([excluded-values message] #(if (contains? excluded-values (%2 %1)) message)))
