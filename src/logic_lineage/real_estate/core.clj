(ns logic-lineage.real-estate.core
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:require [clojure.core.logic.pldb :as pldb]))

;; Example for real estate data domain.

;; First, define the relations as an in-memory database of facts
(pldb/db-rel provider provider-name provider-type)
(pldb/db-rel source data-point provider-name)
(pldb/db-rel derived upstream-data-point downstream-data-point)


(def ia-model
  "The information architecture definition.
  This expresses the data providers, the data structures provided and
  the relations and derivation chains between the fields."
  {:providers
   {:bbr {:type :external
          :provides {:bbr-data {:bbr-land-area []
                                :bbr-house-area []}}}
    :courts {:type :external
             :provides {:etl-data {:etl-sale-price []
                                   :etl-sale-year []}}}
    :dawa {:type :external
           :provides {:dawa-data {:dawa-municipaliy []
                                  :dawa-municipality-geo []}}}
    :data-distributor {:type :internal
                       :provides {:house-data
                                  {:house-land-area [:bbr-land-area]
                                   :house-house-area [:bbr-house-area]
                                   :house-sale-price [:etl-sale-price]
                                   :house-sale-year [:etl-sale-year]
                                   :house-municipality [:dawa-municipality]
                                   :house-municipality-geo [:dawa-municipality-geo]}}}
    :stats {:type :internal
            :provides {:stats-data
                       {:stats-avg-price-per-m2      [:house-house-area :house-sale-price]
                        :stats-avg-price-per-m2-year [:house-sale-year]
                        :stats-avg-price-per-m2-municipality [:house-municipality]}}}
    :appraisal {:type :internal
                :provides {:model-appraisal
                           {:model-score [:house-sale-year :house-municipality]
                            :model-value [:house-land-area :house-house-area
                                          :house-sale-price :house-sale-year
                                          :house-municipality
                                          :stats-avg-price-per-m2
                                          :stats-avg-price-per-m2-year
                                          :stats-avg-price-per-m2-municipality]}}}
    }})
 

(defn facts-from-provider-data
  "Convert a data set provided by a provider into fact relations."
  [pname ptype ds-name ds-def]
  (apply concat  
         (for [[field-name field-def] ds-def]
           (concat [[source field-name pname]]
                  ;; The inputs to derive the field establishes its derivation chain:
                  (for [upstream field-def]
                    [derived upstream field-name])))))
  

(defn facts-from-provider
  "Convert a provider definition to fact relations."
  [pname pdef]
  {:pre  [#(:type pdef #{:external :internal})]}
  (let [ptype (:type pdef)
        provides (:provides pdef)]
    (concat [[provider pname ptype]]
            (apply concat (for [[ds-name ds-def] provides]
                            (facts-from-provider-data pname ptype ds-name ds-def))))))
  
(defn facts-from-ia-model
  "Convert an information architecture model into fact relations."
  [model]
  (apply concat (for [[pname pdef] (:providers model)]
                  (facts-from-provider pname pdef))))

(comment
  (facts-from-ia-model ia-model)
  )

(def facts
  (apply pldb/db
         (facts-from-ia-model ia-model)))

(defn data-points-from-source
  "Find the data points from a given source"
  [db src-name]
  (pldb/with-db db
    (run* [q]
      (fresh [pt]
        (source pt src-name)
        (== q pt)))))

(comment
  (data-points-from-source facts :model)
  ;;=> (:model-score :model-value)
  )

(defn ancestoro
  "Relates a datum to its ancestors (upstream points in the derivation chain)"
  [datum ancestor]
  (conde
   ((derived ancestor datum))
   ((fresh [x]
      (derived x datum)
      (ancestoro x ancestor)))))
            
(defn data-point-ancestors
  "All ancestors of a given data-point (the upstream points in its derivation graph)"
  [db data-point]
  (pldb/with-db db
    (run* [q]
      (fresh [anc]
        (ancestoro data-point anc)
        (== q anc)))))

(comment
  (data-point-ancestors facts :bbr-land-area)
  ;;=> ()

  (data-point-ancestors facts :model-score)
  ;;=> (:house-sale-year :house-municipality :etl-sale-year :dawa-municipality)

  )

(defn data-point-children
  "Get the downstream data-points that are derived from this."
  [db data-point]
  (pldb/with-db db
    (run* [q]
      (fresh [child]
        (ancestoro child data-point)
        (== q child)))))
  
(comment
  ;; See which downstream fields are impacted if there is an error in BBR land area:
  (data-point-children facts :bbr-land-area)
  ;; => (:house-land-area :model-value)
  )

(defn lineage-graph
  [db]
  (pldb/with-db db
    (run* [q]
      (fresh [upstream-point upstream-provider downstream-point downstream-provider]
        (derived upstream-point downstream-point)
        (source upstream-point upstream-provider)
        (source downstream-point downstream-provider)
        (== q [upstream-provider upstream-point downstream-provider downstream-point])))))


(defn point-edges-to-plantuml
  "Format point-edges as a PlantUML graph.
  Edges are represented as a list of tuples,
  (upstream-provider upstream-point downstream-provider downstream-point)."
  [title point-edges]
  (let [provider-point-pairs (->> (for [[upr upt dpr dpt] point-edges]
                                    [[upr upt] [dpr dpt]])
                                  (apply concat)
                                  concat)
        points-by-provider (reduce (fn [state [provider point]]
                                     (update state provider #(conj (set %) point))) {} provider-point-pairs)
        inter-provider-edges (reduce (fn [state [upr _ dpr _]]
                                       (conj state [upr dpr])) #{} point-edges)
        providers (keys points-by-provider)
        ulabel (fn [x] (format "\"%s\"" (if (keyword? x) (name x) (x))))
        uref (fn [x] (-> x
                         (clojure.string/replace #"[:]" "")
                         (clojure.string/replace #"[-]" "X")))
        indent "  "]
    (->> [(str "@startuml")
          (str "  title " title)
          (for [[provider points] points-by-provider]
            [(str indent "package " (ulabel provider) " {")
             (for [point points]
               (str indent indent "object " (ulabel point) " as " (uref point)))
             (str indent "}")])
          (for [[upr upt dpr dpt] point-edges]
            (str indent (uref upt) " --> " (uref dpt)))
          (str "@enduml")]
         flatten
         (interpose (format "%n"))
         (apply str))))
  
(comment
  (->> (lineage-graph facts)
       (point-edges-to-plantuml "Lineage Graph")
       println)
  )
  

(defn impact-graph
  "Get the edges representing the impact of changes to a data-point,
  the point and its related downstream data-points."
  [db data-point]
  (pldb/with-db db
    (run* [q]
      (fresh [parent-point parent-provider child-point child-provider edges]
        ;; child has data-point as ancestor
        (ancestoro child-point data-point)
        ;; parent is a parent of child
        (derived parent-point child-point)
        ;; ..which also has data-point as ancestor
        (ancestoro parent-point data-point)
        ;; Add the provider information
        (source parent-point parent-provider)
        (source child-point child-provider)
        (== q [parent-provider parent-point child-provider child-point])))))


(comment
  (->> (impact-graph facts :bbr-house-area)
       (point-edges-to-plantuml "Impact of changes to BBR house area.")
       println)
  )
