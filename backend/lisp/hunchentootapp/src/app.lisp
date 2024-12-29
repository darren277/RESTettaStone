(load "/quicklisp/setup.lisp")

(ql:quickload '(:hunchentoot :postmodern :cl-json))

(defpackage :web-api
  (:use :cl :hunchentoot :postmodern)
)

(in-package :web-api)

(defun plist-to-alist (plist)
  "Convert a property list (plist) to an association list (alist)."
  (loop for (key value) on plist by #'cddr
        collect (cons key value)))

(defun get-env-port (default-port)
  "Retrieve the port number from the environment variable `PORT` or use the default."
  (let ((env-port (uiop:getenv "PORT")))
    (if env-port
        (parse-integer env-port)
        default-port)))

(defun connect-to-db ()
  "Establish a connection to the PostgreSQL database using environment variables."
  (let ((host (or (uiop:getenv "PG_HOST") "localhost"))
        (port (or (uiop:getenv "PG_PORT") "5432"))
        (db (or (uiop:getenv "PG_DB") "postgres"))
        (user (or (uiop:getenv "PG_USER") "myusername"))
        (pass (or (uiop:getenv "PG_PASS") "mypassword")))
    (postmodern:connect-toplevel db user pass host :port (parse-integer port))))

(defun disconnect-from-db ()
  "Disconnect from the PostgreSQL database."
  (postmodern:disconnect-toplevel))

;; Save a user to the database
(defun save-user (email)
  (postmodern:execute
   "INSERT INTO users (email) VALUES ($1)"
   (format nil "~{~a~^,~}" email)))

;; Fetch all users from the database
(defun fetch-users ()
  "Fetch all users from the database."
  (handler-case
    (let* ((results (postmodern:query "SELECT id, email FROM users"))
       (formatted-results
        (mapcar (lambda (row)
                  (plist-to-alist `(:id ,(first row) :email ,(second row))))results)))
          (setf (hunchentoot:content-type*) "application/json")
          (cl-json:encode-json-to-string formatted-results))
    (error (e)
      (setf (hunchentoot:content-type*) "application/json")
      (cl-json:encode-json-to-string
       `(:status "error"
         :message "Failed to fetch users"
         :details ,(format nil "~A" e))))))

(defun build-json-response (status message &optional user)
  "Build a JSON response with a status, message, and optional user."
  (if user
      (cl-json:encode-json-to-string `(:status ,status :message ,message :user ,user))
      (cl-json:encode-json-to-string `(:status ,status :message ,message))))

;; REST API endpoints for CRUD operations on users
(defun handle-unsupported-method ()
  "Handle unsupported HTTP methods."
  (setf (header-out "Content-Type") "application/json")
  (cl-json:encode-json-to-string `(:status "error" :message "Method not allowed")))

(defun handle-post-user ()
  "Handle the POST request to create a new user."
  (handler-case
    (let* ((raw-data (hunchentoot:raw-post-data :want-stream nil))
           (payload (cl-json:decode-json-from-string
                     (if (typep raw-data '(vector (unsigned-byte 8)))
                         (babel:octets-to-string raw-data)
                         raw-data)))
           ;; Use assoc instead of getf for alist access
           (email (cdr (assoc :email payload))))
      (format t "Decoded payload: ~A~%" payload)
      (format t "Email: ~A~%" email)

      (if (and email)
          (handler-case
            (progn
              (let ((result (first (postmodern:query
                           "INSERT INTO users (email)
                            VALUES ($1)
                            RETURNING id, email"
                           email))))
                (setf (hunchentoot:content-type*) "application/json")
                (cl-json:encode-json-to-string
                 `(:status "success"
                   :message "User created"
                   :data, result))))
            (error (e)
              (setf (hunchentoot:content-type*) "application/json")
              (cl-json:encode-json-to-string
               `(:status "error"
                 :message "Database error occurred"
                 :details ,(format nil "~A" e)))))
          (progn
            (setf (hunchentoot:content-type*) "application/json")
            (cl-json:encode-json-to-string
             `(:status "error"
               :message "Invalid payload - email required")))))
    (error (e)
      (setf (hunchentoot:content-type*) "application/json")
      (cl-json:encode-json-to-string
       `(:status "error"
         :message "Request processing failed"
         :details ,(format nil "~A" e))))))

(hunchentoot:define-easy-handler (users :uri "/users") ()
  (case (hunchentoot:request-method*)
    (:get (fetch-users))
    (:post (handle-post-user))
    (otherwise
     (setf (hunchentoot:content-type*) "application/json")
     (setf (hunchentoot:return-code*) 405)
     (cl-json:encode-json-to-string
      `(:status "error"
        :message "Method not allowed")))))

(defun handle-get-user (id)
  "Fetch and return the user by ID."
  (let ((parsed-id (parse-integer id)))
    (let ((result (first (postmodern:query "SELECT id, email FROM users WHERE id = $1" parsed-id))))
      (if result
          (let* ((user `(:id ,(first result) :email ,(second result)))
                 (user-alist (plist-to-alist user)))
            (setf (header-out "Content-Type") "application/json")
            (cl-json:encode-json-to-string user-alist))
          (progn
            (setf (header-out "Content-Type") "application/json")
            (setf (hunchentoot:return-code*) 404)
            (cl-json:encode-json-to-string `(:status "error" :message "User not found")))))))

(defun handle-update-user (id)
  "Update the user by ID."
  (handler-case
    (let* ((raw-data (hunchentoot:raw-post-data :want-stream nil))
           (payload (cl-json:decode-json-from-string
                     (if (typep raw-data '(vector (unsigned-byte 8)))
                         (babel:octets-to-string raw-data)
                         raw-data)))
           (email (cdr (assoc :email payload)))
           (parsed-id (parse-integer id)))
      (if (and email)
          (handler-case
            (progn
              (let ((result (first (postmodern:query
                            "UPDATE users
                             SET email = $1
                             WHERE id = $2
                             RETURNING id, email"
                            email parsed-id))))
                (if result
                    (progn
                      (let* ((user `(:id ,(first result) :email ,(second result)))
                             (user-alist (plist-to-alist user)))
                        (setf (hunchentoot:content-type*) "application/json")
                        (cl-json:encode-json-to-string user-alist)))
                    (progn
                      (setf (hunchentoot:content-type*) "application/json")
                      (setf (hunchentoot:return-code*) 404)
                      (cl-json:encode-json-to-string
                       `(:status "error"
                         :message "User not found"))))))
            (error (e)
              (setf (hunchentoot:content-type*) "application/json")
              (setf (hunchentoot:return-code*) 500)
              (cl-json:encode-json-to-string
               `(:status "error"
                 :message "Database error occurred"
                 :details ,(format nil "~A" e)))))
          (progn
            (setf (hunchentoot:content-type*) "application/json")
            (setf (hunchentoot:return-code*) 400)
            (cl-json:encode-json-to-string
             `(:status "error"
               :message "Invalid payload - email is required")))))
    (error (e)
      (setf (hunchentoot:content-type*) "application/json")
      (setf (hunchentoot:return-code*) 400)
      (cl-json:encode-json-to-string
       `(:status "error"
         :message "Request processing failed"
         :details ,(format nil "~A" e))))))

(defun handle-delete-user (id)
  "Delete the user by ID."
  (let ((parsed-id (parse-integer id)))
    (postmodern:execute "DELETE FROM users WHERE id = $1" parsed-id)
    (setf (header-out "Content-Type") "application/json")
    (cl-json:encode-json-to-string `(:status "success" :message "User deleted"))))


(hunchentoot:define-easy-handler (user-handler :uri "/users/:id") ()
  (let ((id (car (cl-ppcre:all-matches-as-strings "\\d+" (hunchentoot:script-name*)))))
    (case (hunchentoot:request-method*)
      (:get (handle-get-user id))
      (:put (handle-update-user id))
      (:delete (handle-delete-user id))
      (otherwise
       (setf (hunchentoot:content-type*) "application/json")
       (setf (hunchentoot:return-code*) 405)
       (cl-json:encode-json-to-string
        `(:status "error"
          :message "Method not allowed"))))))

(hunchentoot:define-easy-handler (catch-all :uri "/*") ()
  (let ((response `(:status "error" :message "Unhandled request")))
    (format t "Response plist: ~a~%" response)
    (setf (header-out "Content-Type") "application/json")
    (cl-json:encode-json-to-string response)))

(setf hunchentoot:*dispatch-table*
  (list
        (hunchentoot:create-regex-dispatcher "^/users/([0-9]+)$" 'user-handler)
        (hunchentoot:create-prefix-dispatcher "/users" 'users)
        (hunchentoot:create-prefix-dispatcher "/" 'catch-all)
  ))

(defun start-server (&key (port (get-env-port 8080)))
  "Start the REST API server on the specified port, defaulting to 8080."
  (connect-to-db)
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port port))
  (format t "Dispatch table: ~a~%" hunchentoot:*dispatch-table*)
  (format t "Server running on port ~a~%" port)
  (format t "Server running on port 3038")
  ;; Prevent SBCL from exiting
  (loop (sleep 1)))

(start-server)
