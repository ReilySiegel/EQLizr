#+TITLE: EQLizr
#+OPTIONS: ^:nil

EQLizr keeps all your data on an equal footing.

* Rationale
EQLizr provides a plugin for Pathom Connect that automatically sets up resolvers
for a variety of databases with near-zero configuration
** Why not Walkable?
While walkable is a fantastic library, it does not support Pathom Connect
completely, and requires complex configuration via the floor plan. However,
Walkable is probably more efficient than this library.

* Usage
** Installation
EQLizr can be installed using deps.edn.

#+begin_src clojure
  {:deps {reilysiegel/eqlizr {:git/url "https://github.com/ReilySiegel/EQLizr"
                              :sha     "84a2cde16cbec309f572745e4b8492afc185a8cc"}}}
#+end_src
** PostgreSQL
*Due to its use of ~next.jdbc~, the PostgreSQL backend is only available on JVM*
*Clojure (CLJ).*

Lets say you have a few tables in a PostgreSQL database:

| person.id (Primary Key) | person.first_name | person.last_name |
|-------------------------+-------------------+------------------|
|                       1 | John              | Smith            |
|                       2 | John              | Doe              |


| account.id (Primary Key) | account.email (Unique) | account.person (Unique, Foreign Key) |
|--------------------------+------------------------+--------------------------------------|
|                        1 | john@smith.com         |                                    1 |
|                        2 | john@doe.com           |                                    2 |


| pet.id (Primary Key) | pet.name |
|----------------------+----------|
|                    1 | Spot     |
|                    2 | Buddy    |
|                    3 | Fido     |


| person_pet.id (Primary Key) | person_pet.person (Foreign Key) | person_pet.pet (Foreign Key |
|----------------+--------------------+-----------------|
|              1 |                  1 |               1 |
|              2 |                  1 |               3 |
|              3 |                  2 |               2 |
|              4 |                  2 |               3 |

#+begin_src clojure
  (ns eqlizr.example
    (:require
     [clojure.core.async :as a]
     [com.wsscode.pathom.core :as p]
     [com.wsscode.pathom.connect :as pc]
     [com.wsscode.pathom.sugar :as ps]
     [eqlizr.core :as eqlizr]
     [eqlizr.database :as database]
     [next.jdbc :as jdbc]))

  ;; Set up a connectable object for jdbc.
  ;; This is the connection EQLizr will use to connect to your database.
  (def ds (jdbc/get-datasource {:dbtype "postgresql"
                                :dbname "test"
                                :user   "test"}))

  ;; Add a derivative resolver that requires input from two database fields, and
  ;; constructs a new output from them.
  (pc/defresolver full-name [env {:person/keys [first_name last_name]}]
    {::pc/input  #{:person/first_name :person/last_name}
     ::pc/output [:person/name]}
    {:person/name (str first_name " " last_name)})

  (def parser
    (ps/connect-parallel-parser
     ;; This is a (possibly nested) vector of all Pathom Connect resolvers.
     ;; EQLizr's resolvers, as well as your own, should go in this vector.
     [full-name
      (eqlizr/resolvers {::jdbc/connectable ds
                         ::database/type    :ansi})]))

  ;; Run a query against the parser.
  (a/<!! (parser {} [ ;; Join on all people.
                     {:person/all
                      [;; Request a derived attribute.
                       :person/name
                       ;; One to one join can be done in the same context!
                       :account/email
                       ;; Join with a bridge table.
                       {:person/person_pet
                        [:pet/name]}]}]))
  ;; => #:person{:all
  ;;             [{:person/name "John Smith",
  ;;               :account/email "john@smith.com",
  ;;               :person/person_pet [#:pet{:name "Spot"} #:pet{:name "Fido"}]}
  ;;              {:person/name "John Doe",
  ;;               :account/email "john@doe.com",
  ;;               :person/person_pet [#:pet{:name "Buddy"} #:pet{:name "Fido"}]}]}
#+end_src
*** How it Works

  EQLizr queries the ANSI catalog of your database to find the tables and
  relationships. In doing so, we make a few assumptions about the structure of the
  database.

  - If :table_one/column is a foreign key with a unique constraint to
    :table_two/column, the relationship is treated as one-to-one
  - If :table_one/column is a foreign key without a unique constraint to
    :table_two/column, the relationship is treated as one-to-many
  - Many-to-many relationships are handled as two one-to-many lookups, which is
    why in the example above we join on the bridge table, not the pet table.

** Google Sheets
Yep. EQLizr supports Google Sheets. This example uses [[https://docs.google.com/spreadsheets/d/1EOWjYGWIzf8i7rcnlhvqWtRL5Ke2V2vz7FgYGYL8EBo/edit?usp=sharing][this one]]. Make sure to
read the documentation for [[https://github.com/ReilySiegel/clojure-sheets][clojure-sheets]], as this is a thin wrapper around
that.

#+begin_src clojure
  (ns eqlizr.example
    (:require
     [clojure.core.async :as a]
     [com.wsscode.pathom.core :as p]
     [com.wsscode.pathom.connect :as pc]
     [com.wsscode.pathom.sugar :as ps]
     [eqlizr.core :as eqlizr]
     [eqlizr.database :as database]
     [clojure-sheets.core :as sheets]
     [clojure-sheets.key-fns :as sheets.key-fns]))

  ;; Add a derivative resolver that requires input from two database fields, and
  ;; constructs a new output from them.
  (pc/defresolver full-name [env {:person/keys [first-name last-name]}]
    {::pc/input  #{:person/first-name :person/last-name}
     ::pc/output [:person/name]}
    {:person/name (str first-name " " last-name)})

  (def parser
    (ps/connect-parallel-parser
     ;; This is a (possibly nested) vector of all Pathom Connect resolvers.
     ;; EQLizr's resolvers, as well as your own, should go in this vector.
     [full-name
      (eqlizr/resolvers
       {;; REQUIRED
        ::sheets/id
        "1EOWjYGWIzf8i7rcnlhvqWtRL5Ke2V2vz7FgYGYL8EBo"
        ::database/type       :sheets
        ;; SEMI-OPTIONAL - Uses `::sheets/all` by
        ;; default if `::sheets/primary-key` is also
        ;; left as default. You should set this to
        ;; something more appropriate if you don't set
        ;; `::sheets/primary-key`.
        ::sheets/global-ident :person/all
        ;; OPTIONAL - Default value shown.
        ::sheets/page         1
        ::sheets/unique-keys  #{}
        ::sheets/primary-key  ::sheets/row
        ::sheets/key-fn
        sheets.key-fns/idiomatic-keyword
        ;; CLOJURESCRIPT ONLY - REQUIRED
        ::database/columns
        [:person/first-name :person/last-name :person/address
         :person/address1 :person/address2 :person.address/city
         :person.address/state]})]))

  ;; Run a query against the parser.
  (a/<!! (parser {} [ ;; Join on all people.
                     {:person/all
                      [;; Request a derived attribute
                       :person/name
                       ;; Request an attribute in a different namespace.
                       :person.address/city]}]))
  ;; => #:person{:all
  ;;             [{:person.address/city "Somewhereville",
  ;;               :person/name "Matilda Jones"}
  ;;              {:person.address/city "Somewhereville",
  ;;               :person/name "Michael Jones"}
  ;;              {:person.address/city "Boston",
  ;;               :person/name "Another Person"}
  ;;              {:person.address/city "Noplace",
  ;;               :person/name "Nobody Noname"}]}
#+end_src

Let's take a closer look at just the EQLizr configuration, as it's a bit more
complicated than the PostgreSQL config.
#+begin_src clojure
  (eqlizr/resolvers
   {;; REQUIRED
    ::sheets/id
    "1EOWjYGWIzf8i7rcnlhvqWtRL5Ke2V2vz7FgYGYL8EBo"
    ::database/type       :sheets
    ;; SEMI-OPTIONAL - Uses `::sheets/all` by
    ;; default if `::sheets/primary-key` is also
    ;; left as default. You should set this to
    ;; something more appropriate if you don't set
    ;; `::sheets/primary-key`.
    ::sheets/global-ident :person/all
    ;; OPTIONAL - Default value shown.
    ::sheets/page         1
    ::sheets/unique-keys  #{}
    ::sheets/primary-key  ::sheets/row
    ::sheets/key-fn
    sheets.key-fns/idiomatic-keyword
    ;; CLOJURESCRIPT ONLY - REQUIRED
    ::database/columns
    [:person/first-name :person/last-name :person/address
     :person/address1 :person/address2 :person.address/city
     :person.address/state]})
#+end_src
There are a few things here that aren't self explanatory. First, the
~::database/columns~ entry. This is only required in ClojureScript, so you
should leave it out on the JVM. If you do happen to be in ClojureScript, then
this is a list of all "column" names, after they have been processed by the
~::sheets/key-fn~. Second, ~::sheets/unique-keys~ are unique keys that a
resolver should be generated for.
** Limitations
*** ClojureScript
When using this library with JVM Clojure (CLJ), it requires only a small amount
of configuration, as EQLizr can obtain much of the required information from the
database on startup. However, on ClojureScript (CLJS), EQLizr cannot block on
startup to obtain this information, so it must be provided. If anyone has any
ideas to make this process better, please open an issue.
