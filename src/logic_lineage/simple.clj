(ns logic-lineage.simple
  (:refer-clojure :exclude [==])
   (:use clojure.core.logic))

;; Simple model:
;; 
;; Bread from (Yeast, Water, Flour)
;; Sandwich from (Bread, Cheese, Ham)
;; 


(defn producto [x]
  "A product is an element with no parts, or a composite of products.
   A composite is a product where the parts are also products (composites or elements)."
  (matche [x]
          (;; element
           [[?name ?parts]]
           (emptyo ?parts))
          (;; composite
           [[?name [?parts-first . ?parts-rest]]]
           (distincto [?parts-first ?parts-rest])
           (producto ?parts-first)
           (producto [?name ?parts-rest]))))


(run 5 [q]
  (fresh [p-name p-parts p]
    (membero p-name [:yeast :water :flour])
    (== p [p-name p-parts])
    (producto p)
    (== q p)))


(run* [q]
  (fresh [bread yeast water flour bread-name bread-parts]
    (== yeast [:yeast []])
    (== water [:water []])
    (== flour [:flour []])
    (== bread [bread-name bread-parts])
    (== bread [:bread [yeast water flour]])
    (producto yeast)
    (producto water)
    (producto flour)
    (producto bread)
    (== q bread)))

