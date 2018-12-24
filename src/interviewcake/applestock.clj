(ns interviewcake.applestock)

;; Only one purchase and one sale
(def stock-prices [10 7 5 8 11 9])

(defn naive-max-profit
  "finds max-profit in O(n^2)"
  [[x & xs :as stock-prices]]
  (if-not (seq stock-prices)
    Integer/MIN_VALUE
    (apply max (max-profit xs)
           (map (fn [y] (- y x)) xs))))

(assert (= (naive-max-profit stock-prices) 6))
(assert (= (naive-max-profit [10 7 8 11 5 9]) 4))

(defn max-profit
  "finds max profit in O(n)
  keeps a rolling min and best-profit"
  [stock-prices]
  (->> stock-prices
       (reduce (fn [[best-profit mi] x]
                 (let [new-min (min mi x)
                       new-best-profit (max best-profit (- x mi))]
                   [new-best-profit new-min]))
               [Integer/MIN_VALUE Integer/MAX_VALUE])
       first))

(assert (= (max-profit stock-prices) 6))
(assert (= (max-profit [10 7 8 11 5 9]) 4))
