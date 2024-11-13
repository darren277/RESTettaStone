(ns clojureapp.db
    (:require [clojure.java.jdbc :as j]
                [clojure.string :as str]))


(def pg-db {:dbtype "postgresql"
            :dbname "postgres"
            :host "127.0.0.1"
            :user "myusername"
            :password "mypassword"
            :ssl true
            :sslfactory "org.postgresql.ssl.NonValidatingFactory"})

(defn query [q] j/query pg-db q)
;; (j/query pg-db ["SELECT * FROM users"])


(defn get-users []
    (query ["SELECT * FROM users"]))

(defn get-user [id]
    (query ["SELECT * FROM users WHERE id = ?" id]))

(defn create-user [name]
    (query ["INSERT INTO users (name) VALUES (?)" name]))

(defn update-user [id name]
    (query ["UPDATE users SET name = ? WHERE id = ?" name id]))

(defn delete-user [id]
    (query ["DELETE FROM users WHERE id = ?" id]))

(defn delete-all-users []
    (query ["DELETE FROM users"]))

(defn get-user-by-name [name]
    (query ["SELECT * FROM users WHERE name = ?" name]))



