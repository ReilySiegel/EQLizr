(ns eqlizr.impl.ansi
  "Contains the implementation for ANSI (PostgreSQL) database functions."
  (:require [eqlizr.database :as database]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as result-set]
            [clojure.string :as str]
            [honeysql.core :as sql]
            [eqlizr.resolvers :as resolvers]
            [com.wsscode.pathom.core]
            [com.wsscode.pathom.connect :as pc]))

(def ^:private column-query-ansi
  "This query retrieves all the needed information from the database.
  Here be dragons."
  (sql/format 
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
    :left-join [;; Foreign key joins
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
    :where     [:= :c.table_schema "public"]}
   :allow-namespaced-names? true
   :quoting                 :ansi))


(defmethod database/column-map :ansi [{::jdbc/keys [connectable]}]
  (->> (jdbc/execute! connectable column-query-ansi
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

(defn- generate-global-resolver
  "Given a `column` and `opts`, generate a global resolver that does not require
  input and returns all results in the same table as `column`."
  [{:column/keys [name]}
   {::database/keys [columns]
    ::jdbc/keys     [connectable]}]
  {::pc/sym    (symbol (namespace name) "global")
   ::pc/input  #{}
   ::pc/output [{(keyword (namespace name) "all")
                 (database/columns-in-table columns (namespace name))}]
   ::pc/resolve
   (fn [_ _]
     (let [q (sql/format
              {:select [:*]
               :from   [(keyword (namespace name))]}
              :quoting :ansi)]
       {(keyword (namespace name) "all")
        (jdbc/execute! connectable q)}))})

(defn- generate-unique-resolver
  "Given a `column` and `opts`, generate a resolver that takes a value for
  `column` as input and returns one result. This is used both for primary keys
  and keys with unique constraints."
  [{:column/keys [name]}
   {::database/keys [columns]
    ::jdbc/keys     [connectable]}]
  {::pc/sym    (symbol (namespace name) (str                          
                                         (clojure.core/name name)
                                         "-unique"))
   ::pc/input  #{name}
   ::pc/output (database/columns-in-table columns (namespace name))
   ::pc/resolve
   (fn [_ input]
     (let [name-value (get input name)
           q          (sql/format
                       {:select [:*]
                        :from   [(keyword (namespace name))]
                        :where  [:= name name-value]}
                       :quoting :ansi)]
       (jdbc/execute-one! connectable q)))})

(defn- generate-to-many-resolver
  "Given a `column` and `opts`, generate a global resolver that takes the column's
  `:foreign-name` as input and returns all results with that `:foreign-name`."
  [{:column/keys [name foreign-name]}
   {::database/keys [columns]
    ::jdbc/keys     [connectable]}]
  {::pc/sym    (symbol (namespace name) (str (clojure.core/name name) "-to-many"))
   ::pc/input  #{foreign-name}
   ::pc/output [{(keyword (namespace foreign-name) (namespace name))
                 (database/columns-in-table columns (namespace name))}]
   ::pc/resolve
   (fn [_ input]
     (let [foreign-name-value (get input foreign-name)
           q                  (sql/format
                               {:select [:*]
                                :from   [(keyword (namespace name))]
                                :join   [(keyword (namespace foreign-name))
                                         [:= name foreign-name]]
                                :where  [:= foreign-name foreign-name-value]}
                               :quoting :ansi
                               :allow-namespaced-names? true
                               :namespace-as-table?     true)]
       {(keyword (namespace foreign-name) (namespace name))
        (jdbc/execute! connectable q)}))})

(defmethod resolvers/generate :ansi [{::database/keys [columns]
                                      :as             opts}]
  (into [] (for [{:column/keys [name foreign-name unique? primary-key?]
                  :as          column}
                 (vals columns)]
             (cond
               primary-key?
               [(generate-unique-resolver column opts)
                (generate-global-resolver column opts)]
               ;; One-to-one
               (and unique?
                    (not (nil? foreign-name)))
               [(pc/alias-resolver2 name foreign-name)
                (generate-unique-resolver column opts)]
               ;; one-to-many
               (not (nil? foreign-name))
               [(pc/alias-resolver name foreign-name)
                (generate-to-many-resolver column opts)]
               ;; Unique
               unique?
               (generate-unique-resolver column opts)
               :else []))))
