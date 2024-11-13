(ns clojureapp.core
  (:require [ring.adapter.jetty :as jetty]
            [reitit.ring :as ring]
            [muuntaja.core :as m]
            [reitit.ring.middleware.muuntaja :as muuntaja])
  (:gen-class))

" * '/' Return string on base URL"
" * '/users' Return list of users"
" * '/users/:id Return user for a specific id"
" * '/users POST a user"

(def users (atom {}))

(defn string-handler [_]
  {:status 200
   :body "on the code again"})

(defn create-user [{user :body-params}]
  (let [id (str (java.util.UUID/randomUUID))
        users (->> (assoc user :id id)
                   (swap! users assoc id))]
    {:status 200
     :body users}))

(defn get-users [_]
  {:status 200
   :body @users})

(defn get-user-by-id [{{:keys [id]} :path-params}]
  {:status 200
   :body (get @users id)})

(def app
  (ring/ring-handler
   (ring/router
    ["/"
     ["users/:id" get-user-by-id]
     ["users" {:get get-users
               :post create-user}]
     ["" string-handler]]
    {:data {:muuntaja m/instance
            :middleware [muuntaja/format-middleware]}})))

;(defn start []
;  (run-jetty app {:port  (apply str (System/getenv "PORT"))
;                             :join? false}))

; hardcoded port for testing: 3030
(defn start []
    (jetty/run-jetty app {:port 3030 :join? false}))

(defn -main
  [& args]
  (start))
