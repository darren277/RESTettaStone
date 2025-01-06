(ns user_service.handler
  (:require
    [next.jdbc :as jdbc]
    [cheshire.core :as json]
    [integrant.core :as ig]))

(defmethod ig/init-key :handler
  [_ {:keys [db router]}]
  ;; We wrap the router with a function that merges the db into the request.
  (fn [request]
    (router (assoc request :db db))))

;; GET /users
(defn list-users [req]
  (let [datasource (:db req)
        users (jdbc/execute! datasource ["SELECT id, email FROM users"])]
    {:status 200
     :body   users}))

;; POST /users { "email": "some_email@mail.com" }
(defn create-user [req]
  (let [datasource (:db req)
        {:keys [email]} (:body-params req)
        ;; RETURNING id, email is Postgres-specific to get the inserted row back
        result (jdbc/execute-one!
                 datasource
                 ["INSERT INTO users (email) VALUES (?) RETURNING id, email" email])]
    {:status 200
     :body   result}))

;; GET /users/:id
(defn get-user [req]
  (let [datasource (:db req)
        id         (-> req :path-params :id)
        user       (jdbc/execute-one! datasource
                    ["SELECT id, email FROM users WHERE id = ?" id])]
    (if user
      {:status 200 :body user}
      {:status 404 :body {:error "Not found"}})))

;; PUT /users/:id { "email": "some_email@mail.com" }
(defn update-user [req]
  (let [datasource (:db req)
        id         (-> req :path-params :id)
        {:keys [email]} (:body-params req)
        _          (jdbc/execute-one! datasource
                    ["UPDATE users SET email = ? WHERE id = ?" email id])
        user       (jdbc/execute-one! datasource
                    ["SELECT id, email FROM users WHERE id = ?" id])]
    (if user
      {:status 200 :body user}
      {:status 404 :body {:error "Not found"}})))

;; DELETE /users/:id
(defn delete-user [req]
  (let [datasource (:db req)
        id         (-> req :path-params :id)
        ;; We can RETURNING id to see if a row was indeed deleted
        deleted    (jdbc/execute-one! datasource
                    ["DELETE FROM users WHERE id = ? RETURNING id" id])]
    (if deleted
      {:status 200}
      {:status 404 :body {:error "Not found"}})))
