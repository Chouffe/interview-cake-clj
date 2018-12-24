(ns interviewcake.palindromepermutation
  (:require [clojure.string :as str]))

(defn permutation-check? [s]
  (let [n (count s)
        xs (vals (frequencies s))
        h (group-by odd? xs)
        odds (get h true)
        evens (get h false)]
    (if (even? n)
      (not (seq odds))
      (= (count odds) 1))))

(defn permutation-check-2? [s]
  (->> s
       (reduce (fn [acc c] (if (get acc c) (disj acc c) (conj acc c))) #{})
       count
       (>= 1)))

(assert (permutation-check-2? "civic"))
(assert (permutation-check-2? "ivicc"))
(assert (not (permutation-check-2? "civil")))
(assert (not (permutation-check-2? "livci")))

(assert (permutation-check? "civic"))
(assert (permutation-check? "ivicc"))
(assert (not (permutation-check? "civil")))
(assert (not (permutation-check? "livci")))

;; Brute force solution:
;;  * Length of word: n
;;  * Generate all permutations of the string: O (n!)
;;  * Check whether there is a palindrome: O(n)
;;  * Total time complexity: O(n!n)

(defn permutations
  [xs]
  (if (<= (count xs) 1)
    [xs]
    (apply concat (for [i (range (count xs))
                        :let [y (get xs i)
                              ys (vec (concat (subvec xs 0 i) (subvec xs (+ i 1))))]]
                    (for [perm (permutations ys)] (cons y perm))))))

; (permutations [1])
; (permutations [1 2])
; (permutations [1 2 3])
; (permutations (into [] "civil"))

;; flattening 1 level is done with `(apply concat xs)`

(defn palindrome? [s]
  (= s (str/reverse s)))

(assert (palindrome? "civic"))

(defn naive-permutation-check?
  [s]
  (some palindrome?
        (map #(apply str %) (permutations (vec s)))))

(assert (naive-permutation-check? "civic"))
(assert (naive-permutation-check? "ivicc"))
(assert (not (naive-permutation-check? "civil")))
(assert (not (naive-permutation-check? "livci")))
