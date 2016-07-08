(ns clara.generative-tests.generators
  (:require [clojure.math.combinatorics :as combo]
            [clara.rules :refer :all]
            [schema.core :as sc]))

(sc/defschema SessionOperation {:type (sc/enum :insert :retract :fire)
                                :facts (sc/maybe [sc/Any])})

(defn session-run-ops [session ops]
  (let [final-session (reduce (fn [session op]
                                (let [new-session (condp = (:type op)
                                                    :insert (insert-all session (:facts op))
                                                    :retract (apply retract session (:facts op))
                                                    :fire (fire-rules session))]
                                  new-session))
                              session ops)]))

(defn ^:private retract-before-insertion?
  "Given a sequence of operations, determine if the number of retractions of any fact exceeds the number
  of times has been inserted at any time."
  [ops]
  (let [alter-fact-count (fn [alter-fn]
                           (fn [fc facts]
                             (reduce (fn [updated-fc f]
                                       (update fc f alter-fn))
                                     fc facts)))
        inc-fact-count (alter-fact-count (fnil inc 0))
        dec-fact-count (alter-fact-count (fnil dec 0))

        any-count-negative? (fn [fc]
                              (boolean (some neg? (vals fc))))]
    
    (if (= ::premature-retract (reduce (fn [fact-count op]
                                         (let [new-count (condp = (:type op)
                                                           :insert (inc-fact-count fact-count (:facts op))
                                                           :retract (dec-fact-count fact-count (:facts op))
                                                           fact-count)]
                                           (if (any-count-negative? new-count)
                                             (reduced ::premature-retract)
                                             new-count)))
                                       {}
                                       ops))
      true
      false)))

(defn ops->add-insert-retract [ops dup-level]
  (let [ops->extra (fn [ops]
                     (mapcat (fn [op]
                               (when (= (:type op)
                                        :insert)
                                 [{:type :insert
                                   :facts (:facts op)}
                                  {:type :retract
                                   :facts (:facts op)}]))
                             ops))]
    (apply concat ops
           (repeat dup-level (ops->extra ops)))))

(sc/defn ops->permutations
  [ops :- [SessionOperation]
   {:keys [dup-level] :or {:dup-level 1}}]
  (let [with-dups (for [n (-> dup-level inc range)]
                    (ops->add-insert-retract ops n))
        permutations (apply concat (for [d with-dups]
                                     (combo/permutations d)))]
    ;; The permutation creation allows for a retraction to occur before insertion, which
    ;; effectively removes the retraction from the seq of operations since retractions of facts
    ;; that are not present do not cause alteration of the session state.  The idea of these helpers
    ;; is to produce orders of operations that should all have the same outcomes.
    ;; The following would have an A when done: retract A, insert A
    ;; while this would not:
    ;; insert A, retract A
    ;;
    ;; For now, we can just find all permutations and remove the ones with invalid ordering.
    ;; This is inefficient and there may be a more efficient algorithm or possible implementation.
    (remove retract-before-insertion? permutations)))

        

                
