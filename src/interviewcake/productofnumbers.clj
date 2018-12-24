(ns interviewcake.productofnumbers)


;; Question: You have a list of integers, and for each index you want to find the product of every integer except the integer at that index

;; with division it is easy
(defn product-of-all-ints-except-at-index [xs]
  (let [product (reduce * xs)]
    (reduce (fn [acc x] (conj acc (/ product x))) [] xs)))

;;

(assert (= [84 12 28 21] (product-of-all-ints-except-at-index [1 7 3 4])))

(defn product-of-all-ints-2
  [[x & xs :as coll]]
  {:pre [(>= (count coll) 3)]
   :post [(= (count %) (count coll))]}
  (->> xs
       (reduce (fn [[product ys] y]
                 [(* product y)
                  (conj (mapv (partial * y) ys) product)])
               [x [1]])
       second))

(assert (= [84 12 28 21] (product-of-all-ints-2 [1 7 3 4])))
