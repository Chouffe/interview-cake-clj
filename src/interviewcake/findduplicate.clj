(ns interviewcake.findduplicate)

(defn find-duplicate-v1
  "O(n) space and time"
  [xs]
  (->> xs
       frequencies
       (filter (fn [[k v]] (> v 1)))
       (take 1)
       ffirst
       ))

(find-duplicate-v1 (shuffle [1 2 3 4 1 3 4 5]))

(defn find-duplicate-v2
  "O(n) space and time"
  [xs]
  (reduce (fn [[duplicate s] x]
            (println "duplicate: " duplicate)
            (println "s: " s)
            (println "x: " x)
            (cond
              duplicate [duplicate s]
              (get s x) [x s]
              :else [duplicate (conj s x)]))
          [nil #{}]
          xs))


(find-duplicate-v2 [1 2 3 4 1 3 4 5])

(defn find-duplicate-v3
  "O(n^2) time and O(1) space"
  [xs]
  (some true?
        (for [[i x] (map vector (range) xs)
              [j y] (map vector (range) xs)
              :when (not= i j)]
          (= x y))))

(find-duplicate-v3 [1 2 3 4 1 3 4 5])

;; Sorting the list in place O(nlog n): find the adjacent elements that are equal in O (n)

(defn find-duplicate-v4
  [xs]
  (->> xs
       sort
       (partition 2 1)
       (some (fn [[x y]] (= x y)))
       boolean))

(find-duplicate-v4 [1 2 3 4 1 3 4 5])
(find-duplicate-v4 (range 10))
