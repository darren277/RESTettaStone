(ns user_service.core
  (:require
    [integrant.core :as ig]
    [clojure.java.io :as io]
    [ring.adapter.jetty :as jetty])
  (:gen-class))

(defmethod ig/init-key :server.http/jetty
  [_ {:keys [handler port]}]
  (let [server (jetty/run-jetty handler {:port port :join? false})]
    ;; Return something that integrant can 'halt' if you want
    {:server server}))

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
        ;; Build a Postgres connection string from env
        connection-uri (str "jdbc:postgresql://"
                            (env "PG_HOST" "db") ":"
                            (env "PG_PORT" "5432") "/"
                            (env "PG_DB"   "postgres")
                            "?user="     (env "PG_USER" "postgres")
                            "&password=" (env "PG_PASS" "postgres"))

        ;; console log connection URI...
        _ (println "Connection URI: " connection-uri)

        ;; Optionally parse the :port from env
        port (Integer/parseInt (env "PORT" "3000"))

        _ (println "Port: " port)

        ;; Patch in the environment-derived values
        final-config (-> base-config
                         (assoc-in [:db :connection-uri] connection-uri)
                         (assoc-in [:server.http/jetty :port] port))]
    (ig/init final-config)))
