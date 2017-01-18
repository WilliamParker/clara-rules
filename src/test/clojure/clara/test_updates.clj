(ns clara.test-updates
  (:require [clara.rules :refer :all]
            [clojure.test :refer :all]
            [clara.rules.testfacts :refer :all]
            [clara.rules.engine :as eng]
            [clara.rules.accumulators :as acc]
            [clara.rules.dsl :as dsl]
            [clojure.set :as s]
            [schema.test]
            [clara.test-rules :as tr])
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

        (is (= (frequencies (query update-absent-fact cold-windy-query))
               (-> update-absent-fact
                   fire-rules
                   (query cold-query)
                   frequencies)
               (frequencies [{:?t 15} {:?t 10}]))
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
             (frequencies [{:?t 15} {:?t 10}]))
          "One time update of one fact")

      (is (= (query subsequent-update cold-windy-query)
             (query subsequent-update cold-query)
             [{:?t 15} {:?t 15}])
          "Two distinct updates of one fact each")

      (is (= (query double-update cold-windy-query)
             (query double-update cold-query)
             [{:?t 15} {:?t 15}])
          "One time update of both facts"))))

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
               fire-rules
               (query cold-query))
           [])
        "Test where the replacement ColdAndWindy doesn't meet the alpha condition.")

    (is (= (-> empty-session
               (insert (->ColdAndWindy 10 10))
               fire-rules
               (eng/update-facts [[(->ColdAndWindy 10 10) (->ColdAndWindy -10 -10)]])
               fire-rules
               (query cold-query))
           [{:?t -10}])
        "Test where the original ColdAndWindy doesn't meet the alpha condition.")))

(def ^:private test-unused-field-changes-simple-hash-join-counter (atom 0))
(deftest test-unused-field-changes-simple-hash-join
  (let [cold-temp-rule (dsl/parse-rule [[Temperature (= ?t temperature) (< ?t 0)]]
                                       (do
                                         (swap! test-unused-field-changes-simple-hash-join-counter inc)
                                         (insert! (->Cold ?t))))

        cold-query (dsl/parse-query [] [[Cold (= ?t temperature)]])

        empty-session (mk-session [cold-temp-rule cold-query] :cache false)]

    (reset! test-unused-field-changes-simple-hash-join-counter 0)

    (let [initial-inserted (-> empty-session
                               (insert (->Temperature -10 "MCI"))
                               fire-rules)]

      (is (= (query initial-inserted cold-query)
             [{:?t -10}])
          "Sanity test that the Cold fact is produced without any updates")

      (is (= @test-unused-field-changes-simple-hash-join-counter 1)
          "Sanity test that the counter is incremented by the initial Cold insertion")

      (let [updated-session (-> initial-inserted
                                (eng/update-facts [[(->Temperature -10 "MCI") (->Temperature -10 "LHR")]])
                                fire-rules)]
        (is (= (query updated-session cold-query)
               [{:?t -10}])
            "Check for expect rule outcome after update")

        (is (= @test-unused-field-changes-simple-hash-join-counter 1)
            "Check that we do not have additional RHS activations from the update")

        (is (empty? (-> updated-session
                        (retract (->Temperature -10 "LHR"))
                        fire-rules
                        (query cold-query)))
            "Validate that future truth maintenance based on the updated fact works")))))

(def test-cold-windy-join-update-without-rhs-impact (atom 0))
(deftest test-cold-windy-join-update-without-rhs-impact
  (let [simple-join-rule (dsl/parse-rule [[Temperature (= ?loc location)]
                                          [WindSpeed (= ?loc location)]]
                                         (do
                                           (swap! test-cold-windy-join-update-counter inc)
                                           (insert! (->ColdAndWindy 0 0))))

        filter-join-rule (dsl/parse-rule [[Temperature (= ?loc location)]
                                          [WindSpeed (tr/join-filter-equals ?loc location)]]
                                         (do
                                           (swap! test-cold-windy-join-update-counter inc)
                                           (insert! (->ColdAndWindy 0 0))))

        cw-query (dsl/parse-query [] [[ColdAndWindy (= ?t temperature)]])]

    (doseq [[empty-session join-type] [[(mk-session [simple-join-rule cw-query] :cache false) " simple join"]
                                       [(mk-session [filter-join-rule cw-query] :cache false) " filter join"]]
            
            :let [with-fact-session (-> empty-session
                                        (insert (->Temperature -20 "ORD"))
                                        (insert (->WindSpeed 20 "ORD"))
                                        fire-rules)]]

      (is (= (query with-fact-session cw-query)
             [{:?t 0}])
          (str "Sanity check of our rule without updates for a" join-type))

      (reset! test-cold-windy-join-update-counter 0)

      (is (= (-> with-fact-session
                 (eng/update-facts [[(->Temperature -20 "ORD") (->Temperature -15 "ORD")]])
                 fire-rules
                 (query cw-query))
             [{:?t 0}])
          (str "Update a fact matching the first condition without impacting RHS bindings for a" join-type))

      (is (= @test-cold-windy-join-update-counter 0)
          (str "Validate that an update on the first condition that does not impact RHS bindings does not cause rule activation for a" join-type))

      (reset! test-cold-windy-join-update-counter 0)

      (is (= (-> with-fact-session
                 (eng/update-facts [[(->WindSpeed 20 "ORD") (->WindSpeed 15 "ORD")]])
                 fire-rules
                 (query cw-query))
             [{:?t 0}])
          (str "Update a fact matching the second condition without impacting RHS bindings for a " join-type))

      (is (= @test-cold-windy-join-update-counter 0)
          (str "Validate that an update on the second condition that does not impact RHS bindings does not cause rule activation for a" join-type)))))

(deftest test-cold-windy-join-with-rhs-impact
  (let [simple-join-rule (dsl/parse-rule [[Temperature (= ?loc location) (= ?t temperature)]
                                          [WindSpeed (= ?loc location) (= ?w windspeed)]]
                                         (insert! (->ColdAndWindy ?t ?w)))

        filter-join-rule (dsl/parse-rule [[Temperature (= ?loc location) (= ?t temperature)]
                                          [WindSpeed (tr/join-filter-equals ?loc location) (= ?w windspeed)]]
                                         (insert! (->ColdAndWindy ?t ?w)))

        cw-query (dsl/parse-query [] [[ColdAndWindy (= ?t temperature) (= ?w windspeed)]])]

    (doseq [[empty-session join-type] [[(mk-session [simple-join-rule cw-query] :cache false) " simple join"]
                                       [(mk-session [filter-join-rule cw-query] :cache false) " filter join"]]
            
            :let [with-fact-session (-> empty-session
                                        (insert (->Temperature -20 "ORD"))
                                        (insert (->WindSpeed 20 "ORD"))
                                        fire-rules)]]

      (is (= (query with-fact-session cw-query)
             [{:?t -20 :?w 20}])
          (str "Sanity check of our rule without updates for a" join-type))

      (is (= (-> with-fact-session
                 (eng/update-facts [[(->Temperature -20 "ORD") (->Temperature -10 "ORD")]])
                 fire-rules
                 (query cw-query))
             [{:?t -10 :?w 20}])
          (str "Update of a Temperature fact that is present for a" join-type))

      (is (= (-> with-fact-session
                 (eng/update-facts [[(->WindSpeed 20 "ORD") (->WindSpeed 10 "ORD")]])
                 fire-rules
                 (query cw-query))
             [{:?t -20 :?w 10}])
          (str "Update of a WindSpeed fact that is present for a" join-type))

      (is (= (-> with-fact-session
                 (eng/update-facts [[(->Temperature -10 "ORD") (->Temperature -5 "ORD")]])
                 fire-rules
                 (query cw-query)
                 frequencies)
             (frequencies [{:?t -20 :?w 20} {:?t -5 :?w 20}]))
          (str "Update of a Temperature fact that is not present for a" join-type))

      (is (= (-> with-fact-session
                 (eng/update-facts [[(->WindSpeed 10 "ORD") (->WindSpeed 5 "ORD")]])
                 fire-rules
                 (query cw-query)
                 frequencies)
             (frequencies [{:?t -20 :?w 20} {:?t -20 :?w 5}]))
          (str "Update of a WindSpeed fact that is not present for a" join-type))

      (is (= (-> with-fact-session
                 (eng/update-facts [[(->Temperature -20 "ORD") (->Temperature -20 "LGA")]])
                 fire-rules
                 (query cw-query))
             [])
          (str "Update of a Temperature fact that should cause the join to no longer succeed for a" join-type))

      (is (= (-> with-fact-session
                 (eng/update-facts [[(->WindSpeed 20 "ORD") (->WindSpeed 20 "LGA")]])
                 fire-rules
                 (query cw-query))
             [])
          (str "Update of a WindSpeed fact that should cause the join to no longer succeed for a" join-type)))))

        
                             

    
           

                                        
