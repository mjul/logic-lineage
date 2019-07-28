(ns logic-lineage.real-estate.core-test
  (:require [clojure.test :refer :all]
            [logic-lineage.real-estate.core :refer :all]
            [clojure.string :as str]
            [clojure.core.logic.pldb :as pldb]))


(deftest facts-from-ia-model-test
  (testing "We can parse empty model."
    (let [actual (facts-from-ia-model {:providers {}})]
      (is (empty? actual))))
  (testing "We can parse single-edge model."
    (let [actual (facts-from-ia-model {:providers
                                       {:x-provider {:type :internal
                                                     :provides {:x-data {:x []}}}
                                        :y-provider {:type :internal
                                                     :provides {:y-data {:y [:x]}}}}})]
      (is (not-empty actual))
      ;; two providers, two points, one edge
      (is (= 5 (count actual))))))
  (testing "We can parse the embedded IA model."
    (let [actual (facts-from-ia-model ia-model)]
      (is actual)
      (is (< 5 (count actual)))))


(deftest lineage-graph-test
  (testing "Can  get lineage graph"
    (let [edges (lineage-graph facts)]
      (is (pos? (count edges))))))

(deftest point-edges-to-plantuml-test
  (testing "Can generate PlantUML"
    (let [uml (->> facts
                   lineage-graph
                   (point-edges-to-plantuml "Lineage Graph"))]
      (is (string? uml))
      (is (str/starts-with? uml "@startuml"))
      (is (str/ends-with? uml "@enduml")))))


#_(deftest impact-graph-test
  (testing "Impact for embedded IA model"
    (let [actual (impact-graph facts :bbr-house-area)]
      (testing "should be non-empty"
        (is (not-empty actual)))
      (testing "should be a list of 4-tuples"
        (is (every? #(= 4 (count %)) actual)))
      (testing "should have unique edges"
        (is (= (sort (set actual)) (sort actual))))))
  (testing "Impact for simple X->Y graph"
    (let [model {:providers
                 {:xjob {:type :internal
                         :provides {:x-data {:x []}}}
                  :yjob {:type :internal
                         :provides {:y-data {:y [:x]}}}}}
          db (facts-from-ia-model model)]
      (testing "single impact for upstream node"
        (is (= 1 db))
        (is (= [[:x-provider :x :y]] (impact-graph db :x)))))))
                 
