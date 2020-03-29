(ns eqlizr.database
  (:require [honeysql.core]))

(defmulti column-map
  "Create a map of column names (`:table/column`) to details of those columns.
  Takes an options map `opts` that MUST HAVE `:eqlizr.database/type`, the type
  of database used (currently supports `:ansi` (PostgreSQL)), and
  `:next.jdbc/connectable`, a connectable object that will be passed to
  `next.jdbc`.

  FOR IMPLEMENTERS:
  Column details must be a map containing at least:
  - `:column/name`, the name of the column in `:table/column` format.
  - `:column/foreign-name`, the name of the foreign key, if one exists.
  - `:column/unique?`, true if the column is unique.
  - `:column/primary-key?`, true if the column is a primary key."
  ::type)

(defn columns-in-table
  "Given a column map (from `column-map`) find all columns in the table `table`."
  [column-map table]
  (->> column-map
       vals
       (filter (fn [c] (= (name table) (namespace (:column/name c)))))
       (mapv :column/name)))

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
