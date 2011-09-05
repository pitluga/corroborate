(ns validations.core-spec
  (:use [speclj.core]
        [validations.core]))

(describe "validate"
  (it "validates a simple map"
    (let [simple {:a "hello"}
          errors (validate simple :a #(if (not (= "goodbye" %)) "invalid"))]
      (should= {:a ["invalid"]} errors))))

(describe "is-required"
  (it "returns a validation error if the field is missing"
    (let [example {}
          errors (validate example :field is-required)]
      (should= {:field ["is required"]} errors)))

  (it "returns nothing if the field is provided"
    (let [example {:field "something"}
          errors (validate example :field is-required)]
      (should= {} errors))))
