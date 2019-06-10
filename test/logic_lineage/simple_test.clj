(ns logic-lineage.simple-test
  (:require [clojure.test :refer :all]
            [logic-lineage.simple :refer :all]))


(deftest create-bread-test
  (testing "Can create bread."
    (let [actual (create-bread)]
      (is (= actual [:bread [[:yeast []]
                             [:water []]
                             [:flour []]]])))))



