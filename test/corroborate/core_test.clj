(ns corroborate.core-test
  (:require [clojure.test :as t])
  (:use [corroborate.core]))

(t/deftest validate-validates-a-map
  (t/testing "can take an anonymous validation"
    (t/is (= {:a ["invalid"]}
           (validate {:a "hello"}
                     :a #(if (not (= "goodbye" (%2 %1))) "invalid")))))

  (t/testing "can validate more than one field"
    (t/is (= {:a ["is required"] :b ["is required"] } 
           (validate {}
                     :a (is-required)
                     :b (is-required)))))

  (t/testing "run two validations on a given field"
    (t/is (= {:a ["is required" "is required"] }
           (validate {} 
                     :a [(is-required) (is-required)])))))

(defvalidator validate-foo
  :bar (is-required)
  :baz (is-required))

(t/deftest defvalidator-is-sugar-over-validate
  (t/is (= {:bar ["is required"] :baz ["is required"]}
         (validate-foo {}))))

(t/deftest validate-staged-merges-multiple-validators
  (let [stage1 (fn [model] (validate model :f (is-required)))
        stage2 (fn [model] (validate model :f (is-numeric)))]

    (t/testing "returns errors from the first stage that fails"
      (t/is (= {:f ["is required"]}
             (validate-staged {:f nil} stage1 stage2))))

    (t/testing "will keep running stages until a failure is found"
      (t/is (= {:f ["is not a number"]}
             (validate-staged {:f "b"} stage1 stage2))))

    (t/testing "return nothing if all stages pass"
      (t/is (= {}
             (validate-staged {:f 1} stage1 stage2))))))

(t/deftest is-required-makes-a-field-mandatory
  (t/testing "return a validation error if the field is missing"
    (t/is (= {:field ["is required"]}
           (validate {} :field (is-required)))))

  (t/testing "return nothing if the field is provided"
    (t/is (= {}
           (validate {:field "something"} :field (is-required)))))

  (t/testing "return nothing if the field is a number"
    (t/is (= {}
           (validate {:field 1} :field (is-required)))))

  (t/testing "return nothing if the field is a seq with stuff in it"
    (t/is (= {}
           (validate {:field ["stuff"]} :field (is-required)))))

  (t/testing "return an error if the seq is empty"
    (t/is (= {:field ["is required"]}
           (validate {:field []} :field (is-required)))))

  (t/testing "allow the message to be overriden"
    (t/is (= {:field ["not there"]}
           (validate {} :field (is-required "not there"))))))

(t/deftest is-formatted-validates-a-regular-expression
  (t/testing "return nothing when the string matches the regex"
    (t/is (= {}
           (validate {:f "ac"} :f (is-formatted #"ac")))))

  (t/testing "return an error when the string doesn't match the regex"
    (t/is (= {:f ["is improperly formatted"]}
           (validate {:f "abc"} :f (is-formatted #"ac")))))

  (t/testing "return an error when the value is nil"
    (t/is (= {:f ["is improperly formatted"]}
           (validate {} :f (is-formatted #"regex")))))

  (t/testing "do not return an error when the value is nil and the regex allows blank values"
    (t/is (= {}
           (validate {} :f (is-formatted #"regex|")))))

  (t/testing "allow overriding the message"
    (t/is (= {:f ["bad format"]}
           (validate {:f "abc"} :f (is-formatted #"ac" "bad format"))))))

(t/deftest is-included-in-validates-inclusion-in-a-set
  (t/testing "return nothing when the value is in the set"
    (t/is (= {}
           (validate {:f "a"} :f (is-included-in #{"a" "b" "c"})))))

  (t/testing "return an error when the value is not in the set"
    (t/is (= {:f ["is not included in the list"]}
           (validate {:f "x"} :f (is-included-in #{"a" "b" "c"})))))

  (t/testing "allow overriding the message"
    (t/is (= {:f ["is not a letter"]}
           (validate {:f "1"} :f (is-included-in #{"a" "b" "c"} "is not a letter"))))))

(t/deftest is-excluded-from-validates-exclusion-from-set
  (t/testing "return nothing when the value is not in the set"
    (t/is (={}
          (validate {:f "x"} :f (is-excluded-from #{"a" "b" "c"})))))

  (t/testing "return an error when the value is in the set"
    (t/is (= {:f ["is reserved"]}
           (validate {:f "a"} :f (is-excluded-from #{"a" "b" "c"})))))

  (t/testing "allow overriding the message"
    (t/is (= {:f ["cannot be a, b, or c"]}
           (validate {:f "a"} :f (is-excluded-from #{"a" "b" "c"} "cannot be a, b, or c"))))))

(t/deftest is-confirmed-by-validates-two-fields-match
  (t/testing "return an error when the confirmation does not match"
    (t/is (= {:pwd ["does not match"]}
           (validate {:pwd "p" :pwdc "not p"} :pwd (is-confirmed-by :pwdc)))))

  (t/testing "return nothing when confirmation matches"
    (t/is (= {}
           (validate {:pwd "p" :pwdc "p"} :pwd (is-confirmed-by :pwdc)))))

  (t/testing "allow overriding the message"
    (t/is (= {:pwd ["bad match"]}
           (validate {:pwd "p" :pwdc "not p"} :pwd (is-confirmed-by :pwdc "bad match"))))))

(t/deftest is-numeric-validates-field-is-a-number
  (t/testing "return an error when the value is not a number"
    (t/is (= {:f ["is not a number"]}
           (validate {:f "a"} :f (is-numeric)))))

  (t/testing "return an error when the value is nil"
    (t/is (= {:f ["is not a number"]}
           (validate {:f nil} :f (is-numeric)))))

  (t/testing "returns nothing when the value is a number"
    (t/is (= {}
           (validate {:f 1} :f (is-numeric)))))

  (t/testing "allow overriding the message"
    (t/is (= {:f ["another message"]}
           (validate {:f "a"} :f (is-numeric "another message"))))))

(t/deftest only-if-can-short-circuit-validations
  (t/testing "do not run the validation if guard predicate is false"
    (t/is (= {}
           (validate {:f nil} :f (only-if (fn [_ _] false) (is-required))))))

  (t/testing "run the validation if the guard is true"
    (t/is (= {:f ["is required"] }
           (validate {:f nil} :f (only-if (fn [_ _] true) (is-required)))))))

(t/deftest only-if-not-can-short-circuit-validations
  (t/testing "do not run the validation if guard predicate is true"
    (t/is (= {}
           (validate {:f nil} :f (only-if-not (fn [_ _] true) (is-required))))))

  (t/testing "run the validation if the guard is false"
    (t/is (= {:f ["is required"] }
           (validate {:f nil} :f (only-if-not (fn [_ _] false) (is-required)))))))

(t/deftest allow-nil-skips-validations-if-nil
  (t/testing "do not return an error when the value is nil"
    (t/is (= {}
           (validate {:f nil} :f (allow-nil (is-numeric))))))

  (t/testing "run the underlying validation when the value is not nil"
    (t/is (= {:f ["is not a number"] }
           (validate {:f "a"} :f (allow-nil (is-numeric)))))))
