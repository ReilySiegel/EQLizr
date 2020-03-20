(ns eqlizr.core
  (:require
   [com.wsscode.pathom.core]
   [com.wsscode.pathom.connect :as pc]
   [eqlizr.impl.ansi]
   [eqlizr.database :as database]
   [eqlizr.resolvers :as resolvers]))

(defn plugin
  "Generate resolvers for a database.
  `opts` MUST be a map that contains:

  - `:next.jdbc/connectable`, an object that jdbc can use to connect to the DB
  - `:eqlizr.database/type`, the type of the DB. Currently, only `:ansi` is
     supported."
  [opts]
  (let [column-map (database/column-map opts)]
    {::pc/register (resolvers/generate (assoc opts ::database/columns column-map))}))
