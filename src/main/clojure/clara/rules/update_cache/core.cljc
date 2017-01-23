(ns clara.rules.update-cache.core)

;; Record indicating pending insertion or removal of a sequence of facts.
(defrecord PendingUpdate [type facts])

(defprotocol UpdateCache
  (add-insertions! [this facts])
  (add-retractions! [this facts])
  (get-updates-and-reset! [this]))

(deftype OrderedUpdateCache [updates]

  UpdateCache

  (add-insertions! [this facts]
    (swap! updates into [(->PendingUpdate :insert facts)]))

  (add-retractions! [this facts]
    (swap! updates into [(->PendingUpdate :retract facts)]))

  (get-updates-and-reset! [this]
    (let [current-updates @updates]
      (reset! updates [])
      (partition-by :type current-updates))))

(defn get-ordered-update-cache
  []
  (OrderedUpdateCache. (atom [])))
