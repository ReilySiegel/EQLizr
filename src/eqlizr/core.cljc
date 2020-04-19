(ns eqlizr.core
  (:require
   [com.wsscode.pathom.core]
   [com.wsscode.pathom.connect :as pc]
   #?(:clj [eqlizr.impl.ansi])
   [eqlizr.impl.sheets]
   [eqlizr.resolvers :as resolvers]
   [eqlizr.database :as database]))

(defn plugin
  "Generate resolvers for a database.
  `opts` MUST be a map that contains:
  
  - `:eqlizr.database/type`, the type of the DB.

  Also be sure to include any other map keys that are required for your database
  type."
  [opts]
  (let [column-map (database/column-map opts)]
    {::pc/register (resolvers/generate (assoc opts ::database/columns column-map))}))
