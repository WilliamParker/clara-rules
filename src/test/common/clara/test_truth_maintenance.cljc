#?(:clj
   (ns clara.test-truth-maintenance
     (:require [clara.tools.testing-utils :refer [def-rules-test] :as tu]
               [clara.rules :refer [fire-rules
                                    insert
                                    insert-all
                                    insert!
                                    insert-unconditional!
                                    insert-all-unconditional!
                                    retract
                                    query]]

               [clara.rules.testfacts :refer [->Temperature ->Cold ->WindSpeed
                                              ->TemperatureHistory ->LousyWeather
                                              ->ColdAndWindy]]
               [clojure.test :refer [is deftest run-tests testing use-fixtures]]
               [clara.rules.accumulators :as acc]
               [schema.test :as st])
     (:import [clara.rules.testfacts
               Temperature
               TemperatureHistory
               Cold
               ColdAndWindy
               WindSpeed
               LousyWeather]))

   :cljs
   (ns clara.test-truth-maintenance
     (:require [clara.rules :refer [fire-rules
                                    insert
                                    insert!
                                    insert-unconditional!
                                    insert-all-unconditional!
                                    insert-all
                                    retract
                                    query]]
               [clara.rules.accumulators :as acc]
               [clara.rules.testfacts :refer [->Temperature Temperature
                                              ->TemperatureHistory TemperatureHistory
                                              ->Cold Cold
                                              ->ColdAndWindy ColdAndWindy
                                              ->WindSpeed WindSpeed
                                              ->LousyWeather LousyWeather]]
               [schema.test :as st])
     (:require-macros [clara.tools.testing-utils :refer [def-rules-test]]
                      [cljs.test :refer [is deftest run-tests testing use-fixtures]])))

(use-fixtures :once st/validate-schemas #?(:clj tu/opts-fixture))

(deftest silly-test
  (is true))

(def side-effect-atom (atom nil))

(def-rules-test test-cancelled-activation
  {:rules [cold-rule [[[Temperature (< temperature 20)]]
                      (reset! side-effect-atom ?__token__)]]

   :sessions [empty-session [cold-rule] {}]}

  (reset! side-effect-atom nil)
  (-> empty-session
      (insert (->Temperature 10 "MCI"))
      (retract (->Temperature 10 "MCI"))
      (fire-rules))
  (is (nil? @side-effect-atom)
      "The rule should not fire at all, even with a retraction later, if there is no net fact insertion")

  (reset! side-effect-atom nil)
  (-> empty-session
      (insert (->Temperature 10 "MCI"))
      fire-rules)
  (is (some? @side-effect-atom)
      "Sanity check that the rule fires without a retraction."))

(def-rules-test test-retraction-of-equal-elements
  {:rules [insert-cold [[[Temperature (= ?temp temperature)]]

                        ;; Insert 2 colds that have equal
                        ;; values to ensure they are both
                        ;;retracted
                        (insert! (->Cold ?temp)
                                 (->Cold ?temp))]]

   :queries [find-cold [[] [[?c <- Cold]]]]

   :sessions [empty-session [insert-cold find-cold] {}]}

  (let [;; Each temp should insert 2 colds.
        session-inserted (-> empty-session
                             (insert (->Temperature 50 "LAX"))
                             (insert (->Temperature 50 "MCI"))
                             fire-rules)

        ;; Retracting one temp should retract both of its
        ;; logically inserted colds, but leave the others, even though
        ;; they are equal.
        session-retracted (-> session-inserted
                              (retract (->Temperature 50 "MCI"))
                              fire-rules)]

    (is (= 4 (count (query session-inserted find-cold))))

    (is (= [{:?c (->Cold 50)}
            {:?c (->Cold 50)}
            {:?c (->Cold 50)}
            {:?c (->Cold 50)}]

           (query session-inserted find-cold)))

    (is (= 2 (count (query session-retracted find-cold))))

    (is (= [{:?c (->Cold 50)}
            {:?c (->Cold 50)}]

           (query session-retracted find-cold)))))

(def-rules-test test-insert-and-retract
  {:rules [cold-rule [[[Temperature (< temperature 20) (= ?t temperature)]]
                      (insert! (->Cold ?t))]]

   :queries [cold-query [[] [[Cold (= ?c temperature)]]]]

   :sessions [empty-session [cold-rule cold-query] {}]}

  (let [session (-> empty-session
                    (insert (->Temperature 10 "MCI"))
                    (fire-rules))

        retracted (-> session
                      (retract (->Temperature 10 "MCI"))
                      (fire-rules))]

    (is (= {{:?c 10} 1}
           (frequencies (query session cold-query))))

    ;; Ensure retracting the temperature also removes the logically inserted fact.
    (is (empty?
         (query
          retracted
          cold-query)))))

(def-rules-test test-insert-retract-custom-type

  {:rules [cold-rule [[[:temperature [{value :value}] (< value 20) (= ?t value)]]
                      (insert! {:type :cold :value ?t})]]

   :queries [cold-query [[] [[:cold [{value :value}] (= ?c value)]]]]

   :sessions [empty-session [cold-rule cold-query] {:fact-type-fn :type}]}

  (let [session (-> empty-session
                    (insert {:type :temperature :value 10})
                    (fire-rules))

        retracted (-> session
                      (retract {:type :temperature :value 10})
                      (fire-rules))]

    (is (= {{:?c 10} 1}
           (frequencies (query session cold-query))))

    ;; Ensure retracting the temperature also removes the logically inserted fact.
    (is (empty?
         (query
          retracted
          cold-query)))))

;; Test for issue 67
(def-rules-test test-insert-retract-negation-join

  {:queries [cold-not-windy-query [[] [[Temperature (< temperature 20) (= ?t temperature)]
                                       [:not [WindSpeed]]]]]

   :sessions [empty-session [cold-not-windy-query] {}]}

  (let [session (-> empty-session
                    (insert (->WindSpeed 30 "MCI"))
                    (retract (->WindSpeed 30 "MCI"))
                    (fire-rules))]

    (is (= [{:?t 10}]
           (-> session
               (insert (->Temperature 10 "MCI"))
               (fire-rules)
               (query cold-not-windy-query))))))

(def-rules-test test-unconditional-insert

  {:rules [cold-rule [[[Temperature (< temperature 20) (= ?t temperature)]]
                      (insert-unconditional! (->Cold ?t))]]

   :queries [cold-query [[] [[Cold (= ?c temperature)]]]]

   :sessions [empty-session [cold-rule cold-query] {}]}

  (let [session (-> empty-session
                    (insert (->Temperature 10 "MCI"))
                    (fire-rules))

        retracted-session (-> session
                              (retract (->Temperature 10 "MCI"))
                              fire-rules)]

    (is (= {{:?c 10} 1}
           (frequencies (query session cold-query))))

    ;; The derived fact should continue to exist after a retraction
    ;; since we used an unconditional insert.
    (is (= {{:?c 10} 1}
           (frequencies (query retracted-session cold-query))))))

(def-rules-test test-unconditional-insert-all

  {:rules [cold-lousy-rule [[[Temperature (< temperature 20) (= ?t temperature)]]
                            (insert-all-unconditional! [(->Cold ?t) (->LousyWeather)])]]

   :queries [cold-query [[] [[Cold (= ?c temperature)]]]
             lousy-query [[] [[?l <- LousyWeather]]]]

   :sessions [empty-session [cold-lousy-rule
                             cold-query
                             lousy-query] {}]}

  (let [session (-> empty-session
                    (insert (->Temperature 10 "MCI"))
                    fire-rules)

        retracted-session (-> session
                              (retract (->Temperature 10 "MCI"))
                              fire-rules)]

    (is (= {{:?c 10} 1}
           (frequencies (query session cold-query))))

    (is (= {{:?l (->LousyWeather)} 1}
           (frequencies (query session lousy-query))))

    ;; The derived fact should continue to exist after a retraction
    ;; since we used an unconditional insert.
    (is (= {{:?c 10} 1}
           (frequencies (query retracted-session cold-query))))

    (is (= {{:?l (->LousyWeather)} 1}
           (frequencies (query retracted-session lousy-query))))))

(def-rules-test test-insert-retract-multi-input
  {:rules [cold-windy-rule [[[Temperature (< temperature 20) (= ?t temperature)]
                             [WindSpeed (> windspeed 30) (= ?w windspeed)]]
                            (insert! (->ColdAndWindy ?t ?w))]]

   :queries [cold-windy-query [[] [[ColdAndWindy (= ?ct temperature) (= ?cw windspeed)]]]]

   :sessions [empty-session [cold-windy-rule cold-windy-query] {}]}

  (let [session (-> empty-session
                    (insert (->Temperature 10 "MCI"))
                    (insert (->WindSpeed 40 "MCI"))
                    (fire-rules))

        retracted (-> session
                      (retract session (->Temperature 10 "MCI"))
                      fire-rules)]

    (is (= {{:?ct 10 :?cw 40} 1}
           (frequencies (query session cold-windy-query))))

    ;; Ensure retracting the temperature also removes the logically inserted fact.
    (is (empty?
         (query retracted cold-windy-query)))))

(def-rules-test test-retract-inserted-during-rule
  {:rules [history [[[?temps <- (acc/distinct) :from [Temperature]]]
                    (insert! (->TemperatureHistory ?temps))]

           history-low-salience [[[?temps <- (acc/distinct) :from [Temperature]]]
                                 (insert! (->TemperatureHistory ?temps))
                                 {:salience -10}]

           ;; Rule only for creating data when fired to expose this bug.
           ;; See https://github.com/cerner/clara-rules/issues/54
           create-data [[]
                        (insert! (->Temperature 20 "MCI")
                                 (->Temperature 25 "MCI")
                                 (->Temperature 30 "SFO"))]]

   :queries [temp-query [[] [[?t <- TemperatureHistory]]]]

   ;; The bug this is testing dependend on rule order, so we test
   ;; multiple orders.
   :sessions [session1 [create-data temp-query history] {}
              session2 [history create-data temp-query] {}
              session3 [history temp-query create-data] {}
              session4 [temp-query create-data history] {}

              session5 [create-data temp-query history-low-salience] {}
              session6 [history-low-salience create-data temp-query] {}
              session7 [history-low-salience temp-query create-data] {}
              session8 [temp-query create-data history-low-salience] {}]}

  ;; We should match an empty list to start.
  (is (= [{:?t (->TemperatureHistory #{(->Temperature 20 "MCI")
                                       (->Temperature 25 "MCI")
                                       (->Temperature 30 "SFO")})}]
         
         (-> session1 fire-rules (query temp-query))
         (-> session2 fire-rules (query temp-query))
         (-> session3 fire-rules (query temp-query))
         (-> session4 fire-rules (query temp-query))
         
         (-> session5 fire-rules (query temp-query))
         (-> session6 fire-rules (query temp-query))
         (-> session7 fire-rules (query temp-query))
         (-> session8 fire-rules (query temp-query)))))

  

    

   
