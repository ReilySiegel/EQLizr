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

(def information-schema-query
  "This query retrieves all the needed information about columns and their
  relationships from the database using the information schema.
  Here be dragons."
  {:select    [[#sql/call[:concat :c.table_name "/" :c.column_name]
                :column/name]
               ;; Produces "/" for columns with no foreign key, must be removed
               ;; in post.
               [#sql/call[:concat :ccu.table_name "/" :ccu.column_name]
                :column/foreign-name]
               [#sql/call
                [:or
                 #sql/call[:= "PRIMARY KEY" :tcp.constraint_type]
                 #sql/call[:= "UNIQUE" :tcu.constraint_type]]
                :column/unique?]
               [#sql/call
                [:= "PRIMARY KEY" :tcp.constraint_type]
                :column/primary-key?]]
   :from      [[:information_schema.columns :c]]
   :left-join [ ;; Foreign key joins
               [:information_schema.key_column_usage :kcu]
               [:and
                [:= :kcu.table_name :c.table_name]
                [:= :kcu.column_name :c.column_name]]
               [:information_schema.constraint_column_usage :ccu]
               [:and
                [:= :ccu.constraint_name :kcu.constraint_name]
                [:not [:and
                       [:= :ccu.table_name :kcu.table_name]
                       [:= :ccu.column_name :kcu.column_name]]]]
               ;; Joins for primary key
               [:information_schema.key_column_usage :kcup]
               [:and
                [:= :kcup.table_name :c.table_name]
                [:= :kcup.column_name :c.column_name]]
               [:information_schema.table_constraints :tcp]
               [:and
                [:= :tcp.constraint_type "PRIMARY KEY"]
                [:= :tcp.table_name :c.table_name]
                [:= :kcup.constraint_name :tcp.constraint_name]]
               ;; Joins for unique
               [:information_schema.key_column_usage :kcuu]
               [:and
                [:= :kcuu.table_name :c.table_name]
                [:= :kcuu.column_name :c.column_name]]
               [:information_schema.table_constraints :tcu]
               [:and
                [:= :tcu.constraint_type "UNIQUE"]
                [:= :tcu.table_name :c.table_name]
                [:= :kcuu.constraint_name :tcu.constraint_name]]]
   :where     [:= :c.table_schema "public"]})


(defmethod database/column-map :ansi [{::jdbc/keys [connectable]}]
  (into
   {}
   (comp
    ;; As columns with no foreign key have a :foreign-key value of "/"
    ;; because I don't understand SQL, fix that here.
    (map (fn [c] (update c :column/foreign-name (fn [s]
                                                 (when (not= s "/") s)))))
    ;; Make the names of columns keywords.
    (map (fn [c] (update c :column/foreign-name keyword)))
    (map (fn [c] (update c :column/name keyword)))
    ;; Convert from a vector of columns to a map of names to columns.
    (map (juxt :column/name identity)))
   (jdbc/execute! connectable
                  (sql/format information-schema-query
                              :allow-namespaced-names? true
                              :quoting                 :ansi)
                  {:builder-fn result-set/as-unqualified-modified-maps
                   :label-fn   #(str/replace % #"_" "-")})))

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
