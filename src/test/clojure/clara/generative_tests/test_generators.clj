(ns clara.generative-tests.test-generators
  (:require [clara.generative-tests.generators :refer :all]
            [clojure.test :refer :all]))

(deftest basic-permutations-test
  (let [base-ops [{:type :insert
                   :facts [:a]}]
        permuted-ops (ops->permutations base-ops {:dup-level 1})]
    (is (= (set permuted-ops)
           #{[{:type :insert, :facts [:a]}]
             [{:type :insert, :facts [:a]}
              {:type :insert, :facts [:a]}
              {:type :retract, :facts [:a]}]
             [{:type :insert, :facts [:a]}
              {:type :retract, :facts [:a]}
              {:type :insert, :facts [:a]}]})
        "Basic sanity test that permutations are created correctly.")))
