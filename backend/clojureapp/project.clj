(defproject closureapp "0.1.0-SNAPSHOT"
  :description "ClojureApp"
  :url "https://github.com/darren277/RESTettaStone"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.12.0"]
                 [org.codehaus.plexus/plexus-utils "3.4.1"]
                 [uberdeps/uberdeps "1.4.0" :exclusions [org.codehaus.plexus/plexus-utils]]
                 [org.clojure/java.jdbc "0.7.12"]
                 [jakarta.servlet/jakarta.servlet-api "6.1.0"]
                 [ring/ring-core "1.13.0"]
                 [ring/ring-jetty-adapter "1.13.0"]
                 [com.fasterxml.jackson.datatype/jackson-datatype-jsr310 "2.17.2"]
                 [metosin/reitit "0.7.2" :exclusions [commons-io metosin/jsonista com.fasterxml.jackson.datatype/jackson-datatype-jsr310]]
                 [metosin/reitit-ring "0.7.2"]
                 [metosin/jsonista "0.3.10"]
                 [metosin/muuntaja "0.6.10"]
                 [org.postgresql/postgresql "42.7.4"]]
  :main ^:skip-aot clojureapp.core
  :target-path "target/%s"
  :local-repo ".m2"
  :repl-options {:color false}
  :profiles {:dev {:source-paths ["dev"]
                   :dependencies [[clj-stacktrace "0.2.8"]
                                  [org.clojure/tools.namespace "1.5.0"]
                                  [org.clojure/java.classpath "1.1.0"]]}
             :uberjar {:aot :all}}
  :jvm-opts [
             ;"-Dclojure.server.repl={:port 5556 :accept cljdt/repl :address \"0.0.0.0\"}"
             "-Xms2g"
             "-Xmx2g"
             "-server"])
