(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'user_service)
(def version "0.1.0-SNAPSHOT")
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
;(def uber-file (format "target/%s-%s-standalone.jar" (name lib) version))
(def uber-file "target/myapp-0.1.0-SNAPSHOT-standalone.jar")

(defn uber [_]
  (b/delete {:path "target"})
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (b/compile-clj {:basis basis
                  :class-dir class-dir})
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis basis
           :main 'user_service.core}))
