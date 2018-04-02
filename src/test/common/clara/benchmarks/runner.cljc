(ns clara.benchmarks.runner
  (:require #?(:clj [clara.benchmarks.compilation])))

(defn deref-with-meta
  [benchmark-var]
  (with-meta (deref benchmark-var) (meta benchmark-var)))

(defn find-benchmarks
  []
  (let [benchmark-namespaces ['clara.benchmarks.compilation]
        benchmark-fns (into []
                            (comp
                             (map ns-publics)

                             (mapcat vals)
                             (filter #(-> % meta :benchmark true?))
                             (map deref-with-meta))
                            benchmark-namespaces)]
    benchmark-fns))
    

(defn -main
  []
  (println "I ran"))
