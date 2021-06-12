(defproject netrine "0.1.0-SNAPSHOT"
  :description "A minimalistic, dynamically typed and functional programming language"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :main netrine.clj.core
  :dependencies [[org.clojure/clojure "1.10.3"]]
  :profiles {:dev {:resource-paths ["test/resources"]}})