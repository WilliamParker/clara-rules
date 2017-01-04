(ns clara.test-updates
  (:require [clara.rules :refer :all]
            [clojure.test :refer :all]
            [clara.rules.testfacts :refer :all]
            [clara.rules.engine :as eng]
            [clara.rules.accumulators :as acc]
            [clara.rules.dsl :as dsl]
            [clojure.set :as s]
            [schema.test])
  (import [clara.rules.testfacts Temperature WindSpeed Cold Hot TemperatureHistory
           ColdAndWindy LousyWeather First Second Third Fourth FlexibleFields]))

(deftest test-simple-update
  (let [cold-windy-query (dsl/parse-query [] [[ColdAndWindy (= ?t temperature)]])

        cold-windy-rule (dsl/parse-rule [[ColdAndWindy (= ?t temperature)]]
                                        (insert! (->Cold ?t)))

        cold-query (dsl/parse-query [] [[Cold (= ?t temperature)]])
        
        empty-session (mk-session [cold-windy-query cold-windy-rule cold-query] :cache false)]

    (doseq [[with-initial-fact test-type] [[(-> empty-session
                                                (insert (->ColdAndWindy 10 20))
                                                fire-rules)
                                            " with rules fired on the session before the update"]
                                           [(-> empty-session
                                                (insert (->ColdAndWindy 10 20)))
                                            " with rules not fired on the session before the update"]]]

      (let [update-present-fact (eng/update-facts with-initial-fact [[(->ColdAndWindy 10 20) (->ColdAndWindy 15 20)]])

            update-absent-fact (eng/update-facts with-initial-fact [[(->ColdAndWindy 11 20) (->ColdAndWindy 15 20)]])]

        (is (= (query update-present-fact cold-windy-query)
               (-> update-present-fact
                   fire-rules
                   (query cold-query))
               [{:?t 15}])
            (str "Update of a fact that is present in the session" test-type))

        (is (= (query update-absent-fact cold-windy-query)
               (-> update-absent-fact
                   fire-rules
                   (query cold-query))
               [{:?t 10}])
            (str "Update of fact not present in the session" test-type))))

    ;; Tests validating behavior when the same token exists in the activations queue in the memory
    ;; and in the logical insertion memory.
    (let [with-fired-and-staged (-> empty-session
                                    (insert (->ColdAndWindy 10 20))
                                    fire-rules
                                    (insert (->ColdAndWindy 10 20)))

          single-update (-> with-fired-and-staged
                            (eng/update-facts [[(->ColdAndWindy 10 20) (->ColdAndWindy 15 20)]])
                            fire-rules)

          subsequent-update (-> single-update
                                (eng/update-facts [[(->ColdAndWindy 10 20) (->ColdAndWindy 15 20)]])
                                fire-rules)

          double-update (-> with-fired-and-staged
                            (eng/update-facts [[(->ColdAndWindy 10 20) (->ColdAndWindy 15 20)]
                                               [(->ColdAndWindy 10 20) (->ColdAndWindy 15 20)]])
                            fire-rules)]

      (is (= (frequencies (query single-update cold-windy-query))
             (frequencies (query single-update cold-query))
             (frequencies [{:?t 15} {:?t 10}])))

      (is (= (query subsequent-update cold-windy-query)
             (query subsequent-update cold-query)
             [{:?t 15} {:?t 15}]))

      (is (= (query double-update cold-windy-query)
             (query double-update cold-query)
             [{:?t 15} {:?t 15}])))))

(deftest test-one-fact-in-pair-fails-alpha-condition
  (let [subzero-temp-rule (dsl/parse-rule [[ColdAndWindy (= ?t temperature) (< ?t 0)]]
                                          (insert! (->Cold ?t)))

        cold-query (dsl/parse-query [] [[Cold (= ?t temperature)]])

        empty-session (mk-session [subzero-temp-rule cold-query] :cache false)]

    (is (= (-> empty-session
               (insert (->ColdAndWindy -10 -10))
               fire-rules
               (query cold-query))
           [{:?t -10}])
        "Sanity test of test rules without any updates and a subzero ColdAndWindy")

    (is (= (-> empty-session
               (insert (->ColdAndWindy -10 -10))
               fire-rules
               (eng/update-facts [[(->ColdAndWindy -10 -10) (->ColdAndWindy 10 10)]])
               (query cold-query))
           [])
        "Test where the replacement ColdAndWindy doesn't meet the alpha condition.")))

        
