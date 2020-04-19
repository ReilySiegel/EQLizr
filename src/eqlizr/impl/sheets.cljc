(ns eqlizr.impl.sheets
  (:require [eqlizr.database :as database]
            [eqlizr.resolvers :as resolvers]
            [clojure-sheets.core :as sheets]
            [clojure-sheets.key-fns :as sheets.key-fns]
            [com.wsscode.pathom.core]
            [com.wsscode.pathom.connect :as pc]
            [eqlizr.impl.keyword :as k]
            [clojure.core.async :as a]))

(defmethod database/column-map :sheets
  [{::sheets/keys   [id page key-fn unique-keys primary-key]
    ::database/keys [columns]
    :or             {page        1
                     key-fn       sheets.key-fns/idiomatic-keyword
                     primary-key ::sheets/row
                     unique-keys #{}
                     columns     #?(:clj (into []
                                               (comp (map first)
                                                     (map key-fn))
                                               (a/<!! (sheets/sheet->vecs
                                                       id
                                                       {:page  page
                                                        :key-fn key-fn})))
                                    :cljs [])}}]
  (reduce
   (fn [acc column]
     (assoc acc column
            {:column/name         column
             :column/unique?      (or (= column primary-key)
                                      (unique-keys column))
             :column/primary-key? (= column primary-key)}))
   {}
   ;; Ensure row  is in columns, in case columns are user-specified.
   (distinct (conj columns ::sheets/row))))

(defmethod resolvers/generate-global-resolver :sheets
  [{::database/keys [columns]
    ::sheets/keys   [id page key-fn global-ident]
    :or             {page  1
                     key-fn sheets.key-fns/idiomatic-keyword}}
   {:column/keys [name]}]
  {::pc/sym    (symbol (k/keyword name "all"))
   ::pc/input  #{}
   ::pc/output [{(or global-ident (k/keyword name "all"))
                 (vec (keys columns))}]
   ::pc/resolve
   (fn [_ _]
     (a/go {(or global-ident (k/keyword name "all"))
            (a/<! (sheets/sheet->map id {:page  page
                                         :key-fn key-fn}))}))})

(defmethod resolvers/generate-unique-resolver :sheets
  [{::database/keys [columns]
    ::sheets/keys   [id page key-fn]
    :or             {page  1
                     key-fn sheets.key-fns/idiomatic-keyword}}
   {:column/keys [name]}]
  {::pc/sym    (symbol name)
   ::pc/input  #{name}
   ::pc/output (vec (keys columns))
   ::pc/batch? true
   ::pc/resolve
   (pc/batch-resolver
    (fn [_ inputs]
      (a/go (let [name-values (set (map name inputs))]
              (pc/batch-restore-sort {::pc/inputs inputs
                                      ::pc/key    name}
                                     (filter
                                      (comp name-values name)
                                      (a/<! (sheets/sheet->map
                                             id
                                             {:page  page
                                              :key-fn key-fn}))))))))})
