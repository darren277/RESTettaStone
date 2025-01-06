(ns user_service.server
  (:require
    [integrant.core :as ig]
    [ring.adapter.jetty :as jetty]))

(defmethod ig/init-key :user_service.server.http/jetty
  [_ {:keys [handler port]}]
  (let [server (jetty/run-jetty handler {:port port :join? false})]
    ;; Return something that integrant can 'halt' if you want
    {:server server}))
