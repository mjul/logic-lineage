(ns logic-lineage.real-estate.core-test
  (:require [clojure.test :refer :all]
            [logic-lineage.real-estate.core :refer :all])
  (:require [clojure.string :as str]))

(deftest lineage-graph-test
  (testing "Can  get lineage graph"
    (let [edges (lineage-graph facts)]
      (is (pos? (count edges))))))

(deftest point-edges-to-plantuml-test
  (testing "Can generate PlantUML"
    (let [uml (->> (lineage-graph facts)
                   (point-edges-to-plantuml "Lineage Graph"))]
      (is (string? uml))
      (is (str/starts-with? uml "@startuml"))
      (is (str/ends-with? uml "@enduml")))))


