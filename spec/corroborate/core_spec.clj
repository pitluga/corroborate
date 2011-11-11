(ns corroborate.core-spec
  (:use [speclj.core]
        [corroborate.core]))

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

(describe "validate-staged"
  (let [stage1 (fn [model] (validate model :f (is-required)))
        stage2 (fn [model] (validate model :f (is-numeric)))]

    (it "returns errors from the first stage that fails"
      (let [errors (validate-staged {:f nil} stage1 stage2)]
        (should= {:f ["is required"]} errors)))

    (it "will keep running stages until a failure is found"
      (let [errors (validate-staged {:f "b"} stage1 stage2)]
        (should= {:f ["is not a number"]} errors)))

    (it "return nothing if all stages pass"
      (let [errors (validate-staged {:f 1} stage1 stage2)]
        (should= {} errors)))))

(describe "is-required"
  (it "returns a validation error if the field is missing"
    (let [errors (validate {} :field (is-required))]
      (should= {:field ["is required"]} errors)))

  (it "returns nothing if the field is provided"
    (let [example {:field "something"}
          errors (validate example :field (is-required))]
      (should= {} errors)))

  (it "returns nothing if the field is a number"
    (let [example {:field 1}
          errors (validate example :field (is-required))]
      (should= {} errors)))

  (it "returns nothing if the field is a seq with stuff in it"
    (let [example {:field ["stuff"]}
          errors (validate example :field (is-required))]
      (should= {} errors)))

  (it "returns an error if the seq is empty"
    (let [example {:field []}
          errors (validate example :field (is-required))]
      (should= {:field ["is required"]} errors)))

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

  (it "returns an error when the value is nil"
    (let [errors (validate {} :f (is-formatted #"regex"))]
      (should= {:f ["is improperly formatted"]} errors)))

  (it "does not return an error when the value is nil and the regex allows blank values"
    (let [errors (validate {} :f (is-formatted #"regex|"))]
      (should= {} errors)))

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

(describe "is-excluded-from"
  (it "returns nothing when the value is not in the set"
    (let [errors (validate {:f "x"} :f (is-excluded-from #{"a" "b" "c"}))]
      (should= {} errors)))

  (it "returns an error when the value is in the set"
    (let [errors (validate {:f "a"} :f (is-excluded-from #{"a" "b" "c"}))]
      (should= {:f ["is reserved"]} errors)))

  (it "allows overriding the message"
    (let [errors (validate {:f "a"} :f (is-excluded-from #{"a" "b" "c"} "cannot be a, b, or c"))]
      (should= {:f ["cannot be a, b, or c"]} errors))))

(describe "is-confirmed-by"
  (it "returns an error when the confirmation does not match"
    (let [errors (validate {:pwd "p" :pwdc "not p"} :pwd (is-confirmed-by :pwdc))]
      (should= {:pwd ["does not match"]} errors))) 

  (it "returns nothing when confirmation matches"
    (let [errors (validate {:pwd "p" :pwdc "p"} :pwd (is-confirmed-by :pwdc))]
      (should= {} errors))) 

  (it "allows overriding the message"
    (let [errors (validate {:pwd "p" :pwdc "not p"} :pwd (is-confirmed-by :pwdc "bad match"))]
      (should= {:pwd ["bad match"]} errors))))

(describe "is-numeric"
  (it "returns an error when the value is not a number"
    (let [errors (validate {:f "a"} :f (is-numeric))]
      (should= {:f ["is not a number"]} errors))) 

  (it "returns an error when the value is nil"
    (let [errors (validate {:f nil} :f (is-numeric))]
      (should= {:f ["is not a number"]} errors)))

  (it "returns nothing when the value is a number"
    (let [errors (validate {:f 1} :f (is-numeric))]
      (should= {} errors)))

  (it "allows overriding the message"
    (let [errors (validate {:f "a"} :f (is-numeric "another message"))]
      (should= {:f ["another message"]} errors)))) 

(describe "only-if"
  (it "does not run the validation if guard predicate is false"
    (let [errors (validate {:f nil} :f (only-if (fn [_ _] false) (is-required)))]
      (should= {} errors)))

  (it "runs the validation if the guard is true"
    (let [errors (validate {:f nil} :f (only-if (fn [_ _] true) (is-required)))]
      (should= {:f ["is required"] } errors))))

(describe "only-if-not"
  (it "does not run the validation if guard predicate is true"
    (let [errors (validate {:f nil} :f (only-if-not (fn [_ _] true) (is-required)))]
      (should= {} errors)))

  (it "runs the validation if the guard is false"
    (let [errors (validate {:f nil} :f (only-if-not (fn [_ _] false) (is-required)))]
      (should= {:f ["is required"] } errors))))

(describe "allow-nil"
  (it "does not return an error when the value is nil"
    (let [errors (validate {:f nil} :f (allow-nil (is-numeric)))]
      (should= {} errors)))

  (it "runs the underlying validation when the value is not nil"
    (let [errors (validate {:f "a"} :f (allow-nil (is-numeric)))]
      (should= {:f ["is not a number"] } errors))))
