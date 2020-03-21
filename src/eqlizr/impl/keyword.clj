(ns eqlizr.impl.keyword
  "Utilities for managing table and column names as keywords."
  (:refer-clojure :exclude [keyword]))


(defn keyword
  "A modified version of `clojure.core/keyword` that supports creating keywords
  from components of other keywords.

  Examples:
  (keyword \"namespace\" \"name\")
  ;; => :namespace/name
  (keyword :namespace :name)
  ;; => :namespace/name
  (keyword :table/test :test/column)
  ;; => :table/column"
  ([s] (keyword nil s))
  ([ns s]
   (clojure.core/keyword (when ns
                           (if (qualified-keyword? ns)
                             (namespace ns)
                             (name ns)))
                         (when s
                           (name s)))))

(def table
  "Get the namespace of a keyword as an unqualified keyword."
  (comp keyword namespace))

(def column
  "Get the name of a keyword as an unqualified keyword."
  (comp keyword name))
