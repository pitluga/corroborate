(ns validations.core-spec
  (:use [speclj.core]
        [validations.core]))

(describe "validate"
  (it "can take an anonymous validation"
    (let [simple {:a "hello"}
          errors (validate simple :a #(if (not (= "goodbye" (%2 %1))) "invalid"))]
      (should= {:a ["invalid"]} errors)))

  (it "can validate more than one field"
    (let [errors (validate {} :a (is-required) :b (is-required))]
      (should= {:a ["is required"] :b ["is required"] } errors)))

  (it "run two validations on a given field"
    (let [errors (validate {} :a [(is-required) (is-required)])]
      (should= {:a ["is required" "is required"] } errors))))

(describe "is-required"
  (it "returns a validation error if the field is missing"
    (let [errors (validate {} :field (is-required))]
      (should= {:field ["is required"]} errors)))

  (it "returns nothing if the field is provided"
    (let [example {:field "something"}
          errors (validate example :field (is-required))]
      (should= {} errors)))

  (it "allows the message to be overriden"
    (let [errors (validate {} :field (is-required "not there"))]
      (should= {:field ["not there"]} errors))))
