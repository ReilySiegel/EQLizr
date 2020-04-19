(ns eqlizr.resolvers
  (:require [com.wsscode.pathom.core]
            [com.wsscode.pathom.connect :as pc]
            [eqlizr.database :as database]))

(defmulti generate-unique-resolver
  "Given a `db` and `column`, generate a resolver that takes a value for
  `column` as input and returns one result. This is used both for primary keys
  and keys with unique constraints."
  {:arglists '([db column])}
  (fn [db _] (::database/type db)))

(defmulti generate-global-resolver
  "Given a `db` and `column`, generate a global resolver that does not require
  input and returns all results in the same table as `column`."
  {:arglists '([db column])}
  (fn [db _] (::database/type db)))

(defmulti generate-to-many-resolver
  "Given a `db` and `column`, generate a global resolver that takes the column's
  `:foreign-name` as input and returns all results with that `:foreign-name`."
  {:arglists '([db column])}
  (fn [db _] (::database/type db)))


(defn generate [{::database/keys [columns]
                 :as             db}]
  (into [] (for [{:column/keys [name foreign-name unique? primary-key?]
                  :as          column}
                 (vals columns)]
             (cond
               primary-key?
               [(generate-unique-resolver db column)
                (generate-global-resolver db column)]
               ;; One-to-one
               (and unique?
                    (not (nil? foreign-name)))
               [(pc/alias-resolver2 name foreign-name)
                (generate-unique-resolver db column)]
               ;; one-to-many
               (not (nil? foreign-name))
               [(pc/alias-resolver name foreign-name)
                (generate-to-many-resolver db column)]
               ;; Unique
               unique?
               (generate-unique-resolver db column)
               :else []))))
