(ns clara.test-infinite-loops
  (:require [clojure.test :refer :all]
            [clara.rules :refer :all]
            [clara.rules.testfacts :refer [->Cold  ->Hot ->First ->Second]]
            [clara.tools.testing-utils :refer [def-rules-test
                                               ex-data-maps
                                               side-effect-holder-fixture
                                               side-effect-holder
                                               assert-ex-data]]
            [clara.tools.tracing :as tr]
            [clara.rules.accumulators :as acc])
  (:import [clara.rules.testfacts Cold Hot First Second]
           [clara.tools.tracing
            PersistentTracingListener]))

(use-fixtures :each side-effect-holder-fixture)

(def-rules-test test-truth-maintenance-loop

  ;; Test of an infinite loop to an endless cycle caused by truth maintenance,
  ;; that is an insertion that causes its support to be retracted.

  {:rules [hot-rule [[[:not [Hot]]]
                     (insert! (->Cold nil))]

           cold-rule [[[Cold]]
                      (insert! (->Hot nil))]]

   :sessions [empty-session [hot-rule cold-rule] {}]}

  (assert-ex-data  {:clara-rules/infinite-loop-suspected true}  (fire-rules empty-session {:max-cycles 3000})))

(def-rules-test test-truth-maintenance-loop-with-salience

  ;; Test of an infinite loop to an endless cycle caused by truth maintenance,
  ;; that is an insertion that causes its support to be retracted, when the
  ;; two rules that form the loop have salience and are thus parts of different
  ;; activation groups.

  
  {:rules [hot-rule [[[:not [Hot]]]
                     (insert! (->Cold nil))
                     {:salience 1}]

           cold-rule [[[Cold]]
                      (insert! (->Hot nil))
                      {:salience 2}]]

   :sessions [empty-session [hot-rule cold-rule] {}
              empty-session-negated-salience [hot-rule cold-rule] {:activation-group-fn (comp - :salience :props)}]}

  (doseq [session [empty-session empty-session-negated-salience]]
    ;; Validate that the results are the same in either rule ordering.
    (assert-ex-data  {:clara-rules/infinite-loop-suspected true}  (fire-rules session {:max-cycles 3000}))))


(def-rules-test test-recursive-insertion

  ;; Test of an infinite loop due to runaway insertions without retractions.

  {:rules [cold-rule [[[Cold]]
                      (insert! (->Cold "ORD"))]]

   :sessions [empty-session [cold-rule] {}]}

  (assert-ex-data {:clara-rules/infinite-loop-suspected true}
                  (-> empty-session
                      (insert (->Cold "ARN"))
                      ;; Use a small value here to ensure that we don't run out of memory before throwing.
                      ;; There is unfortunately a tradeoff where making the default number of cycles allowed
                      ;; high enough for some use cases will allow others to create OutOfMemory errors in cases
                      ;; like these but we can at least throw when there is enough memory available to hold the facts
                      ;; created in the loop.
                      (fire-rules {:max-cycles 10}))))

(def-rules-test test-recursive-insertion-loop-no-salience

  {:rules [first-rule [[[First]]
                       (insert! (->Second))]

           second-rule [[[Second]]
                        (insert! (->First))]]

   :sessions [empty-session [first-rule second-rule] {}]}

  (assert-ex-data {:clara-rules/infinite-loop-suspected true}
                  (-> empty-session
                      (insert (->First))
                      (fire-rules {:max-cycles 10}))))

(def-rules-test test-recursive-insertion-loop-with-salience

  {:rules [first-rule [[[First]]
                       (insert! (->Second))
                       {:salience 1}]

           second-rule [[[Second]]
                        (insert! (->First))
                        {:salience 2}]]

   :sessions [empty-session [first-rule second-rule] {}
              empty-session-negated-salience [first-rule second-rule] {:activation-group-fn (comp - :salience :props)}]}

  (doseq [session [empty-session empty-session-negated-salience]]
    (assert-ex-data {:clara-rules/infinite-loop-suspected true}
                    (-> session
                        (insert (->First))
                        (fire-rules {:max-cycles 10})))))

(def-rules-test test-tracing-infinite-loop

  {:rules [cold-rule [[[Cold]]
                      (insert! (->Cold "ORD"))]]

   :sessions [empty-session [cold-rule] {}]}

  (try
    (do (-> empty-session
            tr/with-tracing
            (insert (->Cold "ARN"))
            ;; Use a small value here to ensure that we don't run out of memory before throwing.
            ;; There is unfortunately a tradeoff where making the default number of cycles allowed
            ;; high enough for some use cases will allow others to create OutOfMemory errors in cases
            ;; like these but we can at least throw when there is enough memory available to hold the facts
            ;; created in the loop.
            (fire-rules {:max-cycles 10}))
        (is false "The infinite loop did not throw an exception."))
    (catch Exception e
      (let [data-maps (ex-data-maps e)
            loop-data (filter :clara-rules/infinite-loop-suspected data-maps)]
        (is (= (count loop-data)
               1)
            "There should only be one exception in the chain from infinite rules loops.")
        (is (= (-> loop-data first  :listeners count) 1)
            "There should only be one listener.")
        (is (-> loop-data first  :listeners ^PersistentTracingListener first .-trace not-empty)
            "There should be tracing data available when a traced session throws an exception on an infinite loop.")))))

(def-rules-test test-max-cycles-respected

  ;; As the name suggests, this is a test to validate that setting the max-cycles to different
  ;; values actually works.  The others tests are focused on validating that infinite loops
  ;; throw exceptions, and the max-cycles values there are simply set to restrict resource usage,
  ;; whether CPU or memory, by the test.  Here, on the other hand, we construct a rule where we control
  ;; how many rule cycles will be executed, and then validate that an exception is thrown when that number is
  ;; greater than the max-cycles and is not when it isn't.  For good measure, we validate that the non-exception-throwing
  ;; case has queryable output to make sure that the logic was actually run, not ommitted because of a typo or similar.

  {:rules [recursive-rule-with-end [[[First]]
                                    (when (< @side-effect-holder 20)
                                      (do
                                        (insert-unconditional! (->First))
                                        (swap! side-effect-holder inc)))]]

   :queries [first-query [[] [[?f <- First]]]]

   :sessions [empty-session [recursive-rule-with-end first-query] {}]}


  (reset! side-effect-holder 0)

  (assert-ex-data {:clara-rules/infinite-loop-suspected true}
                  (-> empty-session
                      (insert (->First))
                      (fire-rules {:max-cycles 10})))

  (reset! side-effect-holder 0)

  (is (= (count (-> empty-session
                    (insert (->First))
                    (fire-rules {:max-cycles 30})
                    (query first-query)))
         21)))
