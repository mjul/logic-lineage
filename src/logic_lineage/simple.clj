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
   A composite is a product where the parts are also products (composites or elements).

   Products are internally represented as a tuple of name and constituent parts.
   "
  (matche [x]
          (;; element
           [[?name ?parts]]
           (emptyo ?parts))
          (;; composite
           [[?name [?parts-first . ?parts-rest]]]
           (distincto [?parts-first ?parts-rest])
           (producto ?parts-first)
           (producto [?name ?parts-rest]))))


(defn product-nameo [product product-name]
  "Relates a product to its name."
  (matche [product]
          ([[?name ?parts]]
           (== product-name ?name))))

(run 5 [q]
  (fresh [p-name p-parts p x]
    (membero p-name [:yeast :water :flour])
    (ingrediento p-name p)
    (product-nameo p x)
    (== q x)))



(defn ingrediento [i-name product]
  "Relates a name of a basic (non-composite) ingredient to the
  corresponding product.

  For example

     (ingrediento :yeast [:yeast []])
  "
  (matche [product]
          ([[?name ?parts]]
           (emptyo ?parts)
           (== i-name ?name)
           (producto product))))


(run 5 [q]
  (fresh [p-name p-parts p]
    (membero p-name [:yeast :water :flour])
    (ingrediento p-name p)
    (== q p)))

(defn create-bread []
  (first
   (run* [q]
     (fresh [bread yeast water flour bread-name bread-parts]
       (ingrediento :yeast yeast)
       (ingrediento :water water)
       (ingrediento :flour flour)
       (== bread [bread-name bread-parts])
       (== bread [:bread [yeast water flour]])
       (producto yeast)
       (producto water)
       (producto flour)
       (producto bread)
       (== q bread)))))
  
