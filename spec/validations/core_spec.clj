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

(describe "is-formatted"
  (it "returns nothing when the string matches the regex"
    (let [errors (validate {:f "ac"} :f (is-formatted #"ac"))]
      (should= {} errors)))

  (it "returns an error when the string doesn't match the regex"
    (let [errors (validate {:f "abc"} :f (is-formatted #"ac"))]
      (should= {:f ["is improperly formatted"]} errors)))

  (it "allows overriding the message"
    (let [errors (validate {:f "abc"} :f (is-formatted #"ac" "bad format"))]
      (should= {:f ["bad format"]} errors))))

(describe "is-included-in"
  (it "returns nothing when the value is in the set"
    (let [errors (validate {:f "a"} :f (is-included-in #{"a" "b" "c"}))]
      (should= {} errors)))

  (it "returns an error when the value is not in the set"
    (let [errors (validate {:f "x"} :f (is-included-in #{"a" "b" "c"}))]
      (should= {:f ["is not included in the list"]} errors)))

  (it "allows overriding the message"
    (let [errors (validate {:f "1"} :f (is-included-in #{"a" "b" "c"} "is not a letter"))]
      (should= {:f ["is not a letter"]} errors))))
