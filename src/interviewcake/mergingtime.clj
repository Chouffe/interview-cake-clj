(ns interviewcake.mergingtime)

;; Do not assume it is sorted
(def meetings [[0 1] [3 5] [4 8] [10 12] [9 10]])

(defn merge-if-overlap
  "merge meeting1 `m1` with meeting2 `m2`
  invariant `from1 < from2`
  return a coll of meetings merged or not"
  [[from1 to1 :as m1] [from2 to2 :as m2]]
  (if (<= from2 to1)
    [[from1 (max to1 to2)]]
    [m1 m2]))

(assert (= [[0 1] [3 5]] (merge-if-overlap [0 1] [3 5])))
(assert (= [[0 5]]       (merge-if-overlap [0 1] [1 5])))

(defn merge-sorted-meetings
  "merge sorted ms by `from` in O(n)"
  [[m0 & mr :as sorted-ms]]
  (->> mr
       (reduce (fn [[m1 & ms :as acc] m2]
                 (concat (reverse (merge-if-overlap m1 m2)) ms))
               [m0])
       reverse))

(def merge-meetings  ;; O (n log n)
  (comp merge-sorted-meetings
        (partial sort-by first)))

(assert (= [[0 1] [3 8] [9 12]] (merge-meetings meetings)))
