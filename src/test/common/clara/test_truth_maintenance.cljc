#?(:clj
   (ns clara.test-truth-maintenance
     (:require [clara.tools.testing-utils :refer [def-rules-test] :as tu]
               [clara.rules :refer [fire-rules
                                    insert
                                    insert-all
                                    insert!
                                    retract
                                    query]]

               [clara.rules.testfacts :refer [->Temperature ->Cold ->WindSpeed
                                              ->TemperatureHistory]]
               [clojure.test :refer [is deftest run-tests testing use-fixtures]]
               [clara.rules.accumulators :as acc]
               [schema.test :as st])
     (:import [clara.rules.testfacts
               Temperature
               TemperatureHistory
               Cold
               WindSpeed]))

   :cljs
   (ns clara.test-truth-maintenance
     (:require [clara.rules :refer [fire-rules
                                    insert
                                    insert!
                                    insert-all
                                    retract
                                    query]]
               [clara.rules.accumulators :as acc]
               [clara.rules.testfacts :refer [->Temperature Temperature
                                              ->TemperatureHistory TemperatureHistory
                                              ->Cold Cold
                                              ->WindSpeed WindSpeed]]
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
  
   

  

            
