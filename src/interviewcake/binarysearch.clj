(ns interviewcake.binarysearch)

(defn binary-search
  "returns boolean"
  [e xs]
  (if (not (seq xs))
    false
    (let [n (count xs)
          i (int (/ n 2))
          middle (get xs i)
          left (subvec xs 0 i)
          right (subvec xs (+ i 1))]
      (cond
        (= middle e) true
        (< middle e) (recur e right)
        (> middle e) (recur e left)))))

(binary-search 54 [1 3 5 8 29 54 130 456])

(defn binary-search
  [e xs]
  (if (not (seq xs))
    false
    (let [n (count xs)
          i (int (/ n 2))
          middle (get xs i)
          left (subvec xs 0 i)
          right (subvec xs (inc i))]

      ;; Debugging
      ; (println "n: " n)
      ; (println "i: " i)
      ; (println "middle: " middle)
      ; (println "left " left)
      ; (println "right " right)

      (cond
        (= e middle) true
        (< e middle) (recur e left)
        (> e middle) (recur e right)))))

(binary-search 54 [1 3 5 8 29 54 130 456])

(defn- binary-search-index-aux
  "returns [found? idx]
  idx can be nil if not found"
  [left-idx right-idx e xs]
  (let [n (- right-idx left-idx)]
    (if (<= n 0)
      [false nil]
      (let [i (+ left-idx (int (/ n 2)))
            middle (get xs i) ]
        (cond
          (= e middle) [true i]
          (< e middle) (recur left-idx i e xs)
          (> e middle) (recur (inc i) right-idx e xs))))))

(defn binary-search-index
  [e xs]
  (binary-search-index-aux 0 (count xs) e xs))

(assert (= [true 0] (binary-search-index 1 [1 3 5 8 29 54 130 456])))
(assert (= [true 6] (binary-search-index 130 [1 3 5 8 29 54 130 456])))
(assert (= [false nil] (binary-search-index 6 [1 3 5 8 29 54 130 456])))
