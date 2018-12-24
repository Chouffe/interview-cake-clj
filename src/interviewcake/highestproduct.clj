(ns interviewcake.highestproduct)

;; Question: Given a list of integers, find the highest product you can get from three of the integers.

(defn naive-highest-product
  "finds highest product of 3 numbers in `xs` in O(n^3)"
  [xs]
  (apply max
         (for [[i x] (map vector (range) xs)
               [j y] (map vector (range) xs)
               [k z] (map vector (range) xs)
               :when (and (not (= i j))
                          (not (= j k))
                          (not (= k i)))]
           (* x y z))))

(assert (= 120 (naive-highest-product [1 2 3 4 5 6])))

(defn naive-2-highest-product
  "finds highest product of 3 numbers in `xs` in O(n log n)"
  [xs]
  (->> xs (sort >) (take 3) (reduce *)))

(assert (= 120 (naive-2-highest-product [1 2 3 4 5 6])))

(defn highest-product
  "finds highest product of 3 numbers in `xs` in O(n)"
  [xs]
  (->> xs
       (reduce (fn [[m1 m2 m3] x]
                 (cond
                   (> x m1)                 [x m1 m2]
                   (and (<= x m1) (> x m2)) [m1 x m2]
                   (and (<= x m2) (> x m3)) [m1 m2 x]
                   :else                    [m1 m2 m3]))
               (repeat 3 Integer/MIN_VALUE))
       (reduce *)))

(assert (= 120 (highest-product (shuffle [1 2 3 4 5 6]))))

;; Gotchas: it only works or positive integers
;; Otherwise, we need to keep track of extra variables like
;; * highest-product-of-3
;; * highest-product-of-2
;; * highest
;; * lowest-product-of-2
;; * lowest
;; Or
;; * highest-1
;; * highest-2
;; * lowest-1
;; * lowest-2
