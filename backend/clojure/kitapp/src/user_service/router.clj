(ns user_service.router
  (:require
    [user_service.handler :as h]
    [integrant.core :as ig]
    [reitit.ring :as ring]
    [reitit.ring.middleware.muuntaja :as muuntaja]
    [muuntaja.core :as m]))

(defmethod ig/init-key :user_service.router/router
  [_ {:keys [routes]}]
  (ring/ring-handler
    (ring/router
        [["/users"
            {:get  h/list-users
             :post h/create-user}]
           ["/users/:id"
            {:get    h/get-user
             :put    h/update-user
             :delete h/delete-user}]]
        {:data {:muuntaja   m/instance
             :middleware [muuntaja/format-middleware]}})
    (ring/create-default-handler)))
