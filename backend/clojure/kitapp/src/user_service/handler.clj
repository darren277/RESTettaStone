(ns user_service.handler
  (:require
    [next.jdbc :as jdbc]
    [cheshire.core :as json]
    [integrant.core :as ig]))

(defmethod ig/init-key :user_service.handler/handler
  [_ {:keys [db router]}]
  (fn [request]
    (router (assoc request :db db))))

(defn strip-users-namespace [m]
  ;; For example: {"users/id" 1, "users/email" "foo"} -> {:id 1 :email "foo"}
  (into {}
        (map (fn [[k v]]
               [(keyword (last (clojure.string/split (name k) #"/"))) v]))
        m))

;; GET /users
(defn list-users [req]
  (let [datasource (:db req)
        users (jdbc/execute! datasource ["SELECT users.id AS id, users.email AS email FROM users"])]
    {:status 200
     :body (map strip-users-namespace users)}
     ))

;; POST /users { "email": "some_email@mail.com" }
(defn create-user [req]
  (let [datasource (:db req)
        {:keys [email]} (:body-params req)
        result (jdbc/execute-one!
                 datasource
                 ["INSERT INTO users (email) VALUES (?) RETURNING id, email" email])]
    {:status 200
     :body result}))

;; GET /users/:id
(defn get-user [req]
  (let [datasource (:db req)
        id-str     (-> req :path-params :id)
        id         (Integer/parseInt id-str)
        user       (jdbc/execute-one! datasource
                    ["SELECT users.id AS id, users.email AS email FROM users WHERE id = ?" id])]
    (if user
      {:status 200 :body (strip-users-namespace user)}
      {:status 404 :body {:error "Not found"}})))

;; PUT /users/:id { "email": "some_email@mail.com" }
(defn update-user [req]
  (let [datasource (:db req)
        id-str     (-> req :path-params :id)
        id         (Integer/parseInt id-str)
        {:keys [email]} (:body-params req)
        _          (jdbc/execute-one! datasource
                    ["UPDATE users SET email = ? WHERE id = ?" email id])
        user       (jdbc/execute-one! datasource
                    ["SELECT users.id AS id, users.email AS email FROM users WHERE id = ?" id])]
    (if user
      {:status 200 :body (strip-users-namespace user)}
      {:status 404 :body {:error "Not found"}})))

;; DELETE /users/:id
(defn delete-user [req]
  (let [datasource (:db req)
        id-str     (-> req :path-params :id)
        id         (Integer/parseInt id-str)
        deleted    (jdbc/execute-one! datasource
                    ["DELETE FROM users WHERE id = ? RETURNING id" id])]
    (if deleted
      {:status 200}
      {:status 404 :body {:error "Not found"}})))
