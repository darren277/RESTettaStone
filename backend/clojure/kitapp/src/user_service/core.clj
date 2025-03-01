(ns user_service.core
  (:require
    [integrant.core :as ig]
    [clojure.java.io :as io]
    [user_service.db]
    [user_service.router]
    [user_service.handler]
    [user_service.server])
  (:gen-class))

(defn env
  "Return the environment variable `key` or `default` if not set."
  [key default]
  (if-let [val (System/getenv key)]
    val
    default))

(defn -main
  "Entry point for running as uberjar in Docker."
  [& _]
  (let [base-config (-> "config.edn"
                        io/resource
                        slurp
                        ig/read-string)

        connection-uri (str "jdbc:postgresql://"
                            (env "PG_HOST" "db") ":"
                            (env "PG_PORT" "5432") "/"
                            (env "PG_DB"   "postgres")
                            "?user="     (env "PG_USER" "postgres")
                            "&password=" (env "PG_PASS" "postgres"))

        port (Integer/parseInt (env "PORT" "3000"))

        _ (println "Port: " port)

        ;; Patch in the environment-derived values
        final-config (-> base-config
                         (assoc-in [:user_service.db/db :connection-uri] connection-uri)
                         (assoc-in [:user_service.server.http/jetty :port] port))]
    (ig/init final-config)))
