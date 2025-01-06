(ns user_service.router
  (:require
    [integrant.core :as ig]
    [reitit.ring :as ring]
    [reitit.ring.middleware.muuntaja :as muuntaja]
    [muuntaja.core :as m]))

(defmethod ig/init-key :user_service.router/router
  [_ {:keys [routes]}]
  (ring/ring-handler
    (ring/router routes
                 {:data {:muuntaja   m/instance
                         :middleware [muuntaja/format-middleware]}})
    (ring/create-default-handler)))
