(ns user_service.db
  (:require
    [integrant.core :as ig]
    [next.jdbc :as jdbc]))

(defmethod ig/init-key :db
  [_ {:keys [connection-uri]}]
  (jdbc/get-datasource connection-uri))
