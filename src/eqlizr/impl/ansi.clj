(ns eqlizr.impl.ansi
  "Contains the implementation for ANSI (PostgreSQL) database functions."
  (:require [eqlizr.database :as database]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as result-set]
            [clojure.string :as str]
            [honeysql.core :as sql]
            [eqlizr.resolvers :as resolvers]
            [com.wsscode.pathom.core]
            [com.wsscode.pathom.connect :as pc]
            [eqlizr.impl.keyword :as k]))

(defmethod database/column-map :ansi [{::jdbc/keys [connectable]}]
  (->> (jdbc/execute! connectable
                      (sql/format database/information-schema-query
                                  :allow-namespaced-names? true
                                  :quoting                 :ansi)
                      {:builder-fn result-set/as-unqualified-modified-maps
                       :label-fn   #(str/replace % #"_" "-")})
       ;; As columns with no foreign key have a :foreign-key value of "/"
       ;; because I don't understand SQL, fix that here.
       (mapv (fn [c] (update c :column/foreign-name (fn [s]
                                                     (when (not= s "/") s)))))
       ;; Make the names of columns keywords.
       (mapv (fn [c] (update c :column/foreign-name keyword)))
       (mapv (fn [c] (update c :column/name keyword)))
       ;; Convert from a vector of columns to a map of names to columns.
       (reduce (fn [acc column] (assoc acc (:column/name column) column)) {})))

(defmethod resolvers/generate-global-resolver :ansi
  [{::database/keys [columns]
    ::jdbc/keys     [connectable]}
   {:column/keys [name]}]
  {::pc/sym    (symbol (k/keyword name "all"))
   ::pc/input  #{}
   ::pc/output [{(k/keyword name "all")
                 (database/columns-in-table columns (k/table name))}]
   ::pc/resolve
   (fn [_ _]
     (let [q (sql/format
              {:select [:*]
               :from   [(k/table name)]}
              :quoting :ansi)]
       {(k/keyword name "all")
        (jdbc/execute! connectable q)}))})

(defmethod resolvers/generate-unique-resolver :ansi
  [{::database/keys [columns]
    ::jdbc/keys     [connectable]}
   {:column/keys [name]}]
  {::pc/sym    (symbol name)
   ::pc/input  #{name}
   ::pc/output (database/columns-in-table columns (k/table name))
   ::pc/batch? true
   ::pc/resolve
   (pc/batch-resolver
    (fn [_ inputs]
      (let [name-values (map name inputs)
            q           (sql/format
                         {:select [:*]
                          :from   [(k/table name)]
                          :where  [:in name name-values]}
                         :quoting :ansi)]
        (pc/batch-restore-sort {::pc/inputs inputs
                                ::pc/key    name}
                               (jdbc/execute! connectable q)))))})

(defmethod resolvers/generate-to-many-resolver :ansi
  [{::database/keys [columns]
    ::jdbc/keys     [connectable]}
   {:column/keys [name foreign-name]}]
  {::pc/sym    (symbol name)
   ::pc/input  #{foreign-name}
   ::pc/output [{(k/keyword foreign-name (k/table name))
                 (database/columns-in-table columns (k/table name))}]
   ::pc/resolve
   (fn [_ input]
     (let [foreign-name-value (get input foreign-name)
           q                  (sql/format
                               {:select [:*]
                                :from   [(k/table name)]
                                :join   [(k/table foreign-name)
                                         [:= name foreign-name]]
                                :where  [:= foreign-name foreign-name-value]}
                               :quoting :ansi
                               :allow-namespaced-names? true
                               :namespace-as-table?     true)]
       {(k/keyword foreign-name (k/table name))
        (jdbc/execute! connectable q)}))})
