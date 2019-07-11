(defproject logic-lineage "0.1.0-SNAPSHOT"
  :description "Modelling Data Lineages With Logic Programming"
  :url "http://mjul.com"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.10.1"]
		 [org.clojure/core.logic "0.8.11"]]
  :repl-options {:init-ns logic-lineage.core}
  :plugins [[lein-ancient "0.6.15"]])

