(ns user_service.router
  (:require
    [integrant.core :as ig]
    [reitit.ring :as ring]
    [reitit.ring.middleware.muuntaja :as muuntaja]
    [reitit.coercion.malli]
    [reitit.ring.coercion :as rrc]
    [muuntaja.core :as m]))

(defmethod ig/init-key :router
  [_ {:keys [routes]}]
  (let [defaults
        {:data
         {
          ;; :coercion   malli/coercion
          :muuntaja   m/instance
          :middleware [muuntaja/format-middleware
                       rrc/coerce-exceptions-middleware
                       rrc/coerce-request-middleware
                       rrc/coerce-response-middleware]}}]
    ;; Return a Ring handler built by reitit
    (ring/ring-handler
      (ring/router routes defaults)
      (ring/create-default-handler))))
