(ns clara.generative.test-accum
  (:require [clara.rules :refer :all]
            [clojure.test :refer :all]
            [clara.rules.testfacts :refer :all]
            [clara.rules.engine :as eng]
            [clara.rules.compiler :as com]
            [clara.rules.accumulators :as acc]
            [clara.rules.dsl :as dsl]
            [clara.tools.tracing :as t]
            [schema.test]
            [clara.generative.generators :as gen]
            [clojure.pprint :refer [pprint]])
  (import [clara.rules.testfacts Temperature WindSpeed Cold Hot TemperatureHistory
           ColdAndWindy LousyWeather First Second Third Fourth FlexibleFields]))

(use-fixtures :once schema.test/validate-schemas)

(deftest test-simple-all-condition-binding-groups-3
  (let [r (dsl/parse-rule [[?ts <- (acc/all) :from [Temperature (= ?loc location)]]]
                          (insert! (->TemperatureHistory [?loc (sort (map :temperature ?ts))])))

        q (dsl/parse-query [] [[?history <- TemperatureHistory]])

        empty-session (mk-session [r q] :cache false)]

    (let [operations [{:type :insert
                       :facts [(->Temperature 11 "MCI")]}
                      {:type :insert
                       :facts [(->Temperature 19 "MCI")]}
                      {:type :insert
                       :facts [(->Temperature 1 "ORD")]}
                      {:type :insert
                       :facts [(->Temperature 22 "LAX")]}
                      {:type :retract
                       :facts [(->Temperature 22 "LAX")]}
                      {:type :fire}]

          operation-permutations (gen/ops->permutations operations {})

          expected-output? (fn [session permutation]
                             (let [actual-temp-hist (-> session
                                                        (query q)
                                                        frequencies)
                                   expected-temp-hist (frequencies [{:?history (->TemperatureHistory ["MCI" [11 19]])}
                                                                    {:?history (->TemperatureHistory ["ORD" [1]])}])]
                               (= actual-temp-hist expected-temp-hist)))]
      (doseq [permutation operation-permutations
              :let [session (gen/session-run-ops empty-session operations)]]
        (is (expected-output? session permutation)
            (str "Failure for operation permutation: "
                 \newline
                 (into [] permutation)
                 \newline
                 "Output was: "
                 \newline
                 (into [] (query session q))))))

    (let [operations (mapcat (fn [fact]
                               [{:type :insert
                                 :facts [fact]}
                                {:type :retract
                                 :facts [fact]}])
                             [(->Temperature 10 "MCI")
                              (->Temperature 20 "MCI")
                              (->Temperature 15 "LGA")
                              (->Temperature 25 "LGA")])

          operation-permutations (gen/ops->permutations operations {})]
      
      (doseq [permutation operation-permutations
              :let [session (gen/session-run-ops empty-session operations)
                    output (query session q)]]
        (is (empty? output)
            (str "Non-empty result for operation permutation: "
                 \newline
                 (into [] permutation)
                 "Output was: "
                 (into [] output)))))))
        
