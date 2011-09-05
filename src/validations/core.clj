(ns validations.core
  (:use [clojure.contrib.string :only [blank?]]))

(defn validate [topic field validation]
  (let [error (validation (topic field))]
    (if (blank? error) {} {field [error]})))

(defn is-required [field]
  (if (blank? field) "is required"))
