(ns eqlizr.resolvers
  (:require [eqlizr.database :as database]))

(defmulti generate
  "Generate resolvers for the database.
  Dispatches based on DB type."
  ::database/type)
