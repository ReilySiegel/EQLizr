(ns eqlizr.database)

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
  (into []
        (comp (filter (fn [c] (= (name table) (namespace (:column/name c)))))
              (map :column/name))
        (vals column-map)))
