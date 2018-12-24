(ns interviewcake.mergesortedarrays)

(defn merge-sorted-vecs
  "merge sorted vecs `v1` and `v2` in O(n)"
  [acc [x & xs :as v1] [y & ys :as v2]]
  (cond
    (not (seq v1)) (concat(reverse acc) v2)   ;; O (n)
    (not (seq v2)) (concat (reverse acc) v1)  ;; O (n)
    (<= x y)       (recur (cons x acc) xs v2)
    (> x y)        (recur (cons y acc) v1 ys)))

;; Test cases
(merge-sorted-vecs nil [1] [4])
(merge-sorted-vecs nil [4] [1])
(merge-sorted-vecs nil [1 2 3] [4 5 6])
(merge-sorted-vecs nil [1 5] [4])
(merge-sorted-vecs nil [4] [1 5])
(merge-sorted-vecs nil [4 4 5 6] [1 5])

(def v1 [3 4 6 10 15])
(def v2 [1 5 8 12 14 19])

(assert (= '(1 3 4 5 6 8 10 12 14 15 19)
           (merge-sorted-vecs nil v1 v2)))

(defn merge-sorted
  "merge several sorted lists together in O(n)"
  [v1 & vs]
  (reduce (partial merge-sorted-vecs nil) v1 vs))

(assert (= (merge-sorted [1 2 3] [4 5 6] [1 4 6])
           (merge-sorted [4 5 6] [1 4 6] [1 2 3])))
