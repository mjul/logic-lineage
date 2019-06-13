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

(defn product-partso [product parts]
  "Relates a product to its parts."
  (matche [product]
          ([[?name ?parts]]
           (== parts ?parts))))

(defn product-nameo [product product-name]
  "Relates a product to its name."
  (matche [product]
          ([[?name ?parts]]
           (== product-name ?name))))

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

(comment
  (run 5 [q]
    (fresh [p-name p-parts p x]
      (membero p-name [:yeast :water :flour])
      (ingrediento p-name p)
      (product-nameo p x)
      (== q x))))

(defn breado [product]
  "Relates a product to the specific Bread product."
  (fresh [yeast water flour bread bname parts]
    (ingrediento :yeast yeast)
    (ingrediento :water water)
    (ingrediento :flour flour) 
    (== parts [yeast water flour])
    (== bread [:bread parts])
    (producto bread)
    (== bread product)))


(defn containso [ingredient product]
  "Relation between an ingredient and a product, true if the product contains that ingredient."
  (producto ingredient)
  (producto product)
  (conde
   ((== product ingredient))
   ((matche [product]
            ([[?name ?parts]]
             (conde
              ((membero ingredient ?parts))
              ((fresh [composite]
                 (membero composite ?parts)
                 (containso ingredient composite)))))))))

(comment
  (run 5 [q]
    (fresh [bread ingredient iname iparts]
      (breado bread)
      (containso ingredient bread)
      (== [iname iparts] ingredient)
      (== q iname))))

(comment
  (run 5 [q]
    (fresh [bread yeast flour ham contains-ham contains-flour]
      (ingrediento :yeast yeast)
      (ingrediento :flour flour)
      (ingrediento :ham ham)
      (breado bread)
      (== contains-ham (containso ham bread))
      (== contains-flour (containso flour bread))
      (== q [bread contains-ham contains-flour]))))

(defn create-bread []
  (first
   (run* [q]
     (fresh [bread]
       (breado bread)
       (producto bread)
       (== q bread)))))

(defn sandwicho [s-name fillings product]
  "Relates a sandwich with name and fillings to the corresponding product."
  (fresh [bread sandwich filling ingredients]
    (breado bread)
    (conso bread fillings ingredients) 
    (product-nameo sandwich s-name)
    (product-partso sandwich ingredients)
    (producto sandwich)
    (== product sandwich)))

(defn create-sandwich []
  (first
   (run* [q]
     (fresh [sandwich ham cheese] 
       (ingrediento :ham ham)
       (ingrediento :cheese cheese)
       (sandwicho :ham-and-cheese-sandwich [ham cheese] sandwich)
       (== q sandwich)))))
