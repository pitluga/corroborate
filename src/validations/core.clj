(ns validations.core
  (:use [clojure.contrib.string :only [blank?]]))

(defn validate [topic & fields-with-validations]
  (loop [[field validation & more] fields-with-validations errors {}]
    (if (nil? field)
      errors
      (let [error (validation (field topic))
            field-errors (conj (field errors) error)]
        (recur more (if (blank? error) errors (conj errors [field field-errors])))))))

(defn is-required
  ([] (is-required "is required"))
  ([message] #(if (blank? %) message)))
