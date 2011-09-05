(ns validations.core
  (:use [clojure.contrib.string :only [blank?]]))

(defn validate [topic & fields-with-validations]
  (loop [[field validation & more] fields-with-validations errors {}]
    (if (nil? field)
      errors
      (let [error (validation (field topic))]
        (recur more (if (blank? error) errors (conj errors [field [error]])))))))

(defn is-required [field]
  (if (blank? field) "is required"))
