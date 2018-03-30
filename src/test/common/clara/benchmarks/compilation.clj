(ns clara.benchmarks.compilation
  (:require [clojure.test :refer :all]
            [clara.rules.compiler :as com]
            [clojure.template :as temp]
            [clara.rules :as r]))

(defrecord CommonRecord [id])

(defn gen-rules
  [num-rules]
  (let [gen-facts (mapv (comp keyword str) (range num-rules))
        names (mapv (comp gensym #(str "rule-" %)) (range num-rules))
        body (interleave gen-facts names)]
    `(temp/do-template
      [~'fact
       ~'name]
      (r/defrule ~'name
        [~'?a ~'<- ~'CommonRecord]
        [~'?b ~'<- ~'fact (= (:id ~'?a) ~'this)]
        ~'=>
        nil)
      ~@body)))

(defn ^{:benchmark true
        :description "Benchmark for issue 377"}
  common-conditions-benchmark
  []
  (r/mk-session [(gen-rules 10000)] :cache false))
