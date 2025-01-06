(ns user_service.db
  (:require
    [integrant.core :as ig]
    [next.jdbc :as jdbc]))

(defmethod ig/init-key :user_service.db/db
  [_ {:keys [connection-uri]}]
  (jdbc/get-datasource connection-uri))
