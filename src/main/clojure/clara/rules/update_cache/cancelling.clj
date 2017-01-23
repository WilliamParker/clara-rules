(ns clara.rules.update-cache.cancelling
  (:require [clara.rules.update-cache.core :as uc])
  (:import [java.util
            List
            Map
            LinkedList
            LinkedHashMap
            Collections]))

;;; We need a wrapper to use Clojure equality semantics inside
;;; a Java collection.  Furthermore, since we know we will need to do
;;; a hash operation for each such wrapper created anyway we can ensure
;;; that if the hashes of two facts are not equal that the equals implementation
;;; here will quickly return false.
(deftype FactWrapper [fact ^int fact-hash]

  Object
  (equals [this other]

    (cond

      (identical? this other)
      true

      (not (== fact-hash (.fact_hash ^FactWrapper other)))
      false

      :else (= fact (.fact ^FactWrapper other))))

  (hashCode [this] fact-hash))


;;; These functions essentially allow us to use a Java map to create a set that stores
;;; the frequency of its items.  Note that when multiple instances of a fact are added
;;; we keep both instances as distinct objects.  We don't strictly speaking need to do this
;;; but we expect it to perform better.  The memory will retain both distinct references
;;; and future updates are expected to be faster if these references are maintained since
;;; memory operations look for matches on identity first in tokens before falling back to matching
;;; on equality.
(defn inc-fact-count! [^Map m fact]
  (let [wrapper (FactWrapper. fact (hash fact))
        ^List current-val (.get m wrapper)]
    (if current-val
      (.add current-val fact)
      (.put m wrapper (java.util.LinkedList. [fact])))))

(defn dec-fact-count! [^Map m fact]
  (let [wrapper (FactWrapper. fact (hash fact))
        ;; Note that we specifically use a LinkedList type hint here since we
        ;; use methods from multiple interfaces here, namely List and Deque.
        ^LinkedList current-val (.get m wrapper)]
    (if current-val
      (do
        (if (= (.size current-val) 1)
          (.remove m wrapper)
          ;;; Since as noted above, the facts are equal, we don't actually care which one we remove.
          ;;; We remove the first here to avoid any work checking equality and since this is a constant-time
          ;;; operation on LinkedList.
          (.removeFirst current-val))
        true)
      false)))

(defn map->vals-concated
  [^Map m]
  (let [val-list (java.util.LinkedList.)
        it (.iterator (.entrySet m))]
    (loop []
      (when (.hasNext it)
        (do (let [^java.util.Map$Entry e (.next it)
                  fact (.fact ^FactWrapper (.getKey e))
                  facts-in-val (.getValue e)]
              (doseq [f facts-in-val]
                (.add val-list f)))
            (recur))))
    ;; This list will never be exposed to the user; it is simply iterated over
    ;; by the engine and then discarded.  This being the case there is no
    ;; need to return a persistent data structure rather than an unmodifiable one.
    (Collections/unmodifiableList val-list)))

;;; This is a pending updates cache that allows
;;  retractions and insertions of equal facts
;;; to cancel each other out.
;;; More formally, for i insertions and r retractions
;;; of a fact f, it will:
;;; - If i = r, no operations will be performed.
;;; - If i > r, f will be returned for insertion i - r times.
;;; - If r > i, f will be returned for retraction r - i times.
(deftype CancellingUpdateCache [^Map ^:unsynchronized-mutable insertions
                                ^Map ^:unsynchronized-mutable retractions]

  uc/UpdateCache

  (add-insertions! [this facts]
    (doseq [fact facts]
      (when-not (dec-fact-count! retractions fact)
        (inc-fact-count! insertions fact))))

  (add-retractions! [this facts]
    (doseq [fact facts]
      (when-not (dec-fact-count! insertions fact)
        (inc-fact-count! retractions fact))))

  (get-updates-and-reset! [this]
    (let [retractions-update (when (-> retractions .size pos?)
                               (uc/->PendingUpdate :retract (map->vals-concated retractions)))
          insertions-update (when (-> insertions .size pos?)
                              (uc/->PendingUpdate :insert (map->vals-concated insertions)))]
      (set! insertions (LinkedHashMap.))
      (set! retractions (LinkedHashMap.))

      (cond

        (and insertions-update retractions-update)
        [[retractions-update] [insertions-update]]

        insertions-update
        [[insertions-update]]

        retractions-update
        [[retractions-update]]))))

;; We use LinkedHashMap so that the ordering of the pending updates will be deterministic.
(defn get-cancelling-update-cache
  []
  (CancellingUpdateCache. (LinkedHashMap.) (LinkedHashMap.)))
