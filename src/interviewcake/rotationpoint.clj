(ns interviewcake.rotationpoint)

;; Question: Write a function for finding the index of the  "rotation point".

;; Naive solution: linear scan through the list. When the next work is < than current word, then we found the rotating point. O(n) time. O(1) space


;; Efficient Solution: Adapt binary search to the problem
;; 1. Half the list and pick middle element `e`
;; name l first element of list in first h1
;; name r first element of list in first h2
;; 2. half list element then recur in second half
;; 3. if e > half list element then recur in first half

;; e and middle only
;; e: first element of list
;;

(defn- find-rotation-point-aux
  [left-idx right-idx xs]
  (let [n (inc (- right-idx left-idx))]
    (cond
      (< n 0) nil
      (and (= 2 n)
           (pos? (compare (get xs left-idx)
                          (get xs right-idx)))) right-idx
      :else (let [i (+ left-idx (int (/ n 2)))
                  e (get xs left-idx)
                  middle (get xs i)]

              ;; Debugging
              ; (println "n: " n)
              ; (println "e: " e)
              ; (println "i: " i)
              ; (println "middle: " middle)
              ; (println "left-idx: " left-idx)
              ; (println "right-idx: " right-idx)

              (cond
                (< (compare e middle) 0) (recur i right-idx xs)
                (> (compare e middle) 0) (recur left-idx i xs)
                ; (> e middle) (recur left-idx i xs)
                ; (< e middle) (recur (inc i) right-idx xs)
                )))))

(defn find-rotation-point
  [xs]
  (find-rotation-point-aux 0 (dec (count xs)) xs))

(assert (= 3 (find-rotation-point ["x" "y" "z" "a" "b" "c" "d"])))
(assert (= 1 (find-rotation-point ["z" "a" "b" "c" "d"])))
(assert (nil? (find-rotation-point ["a" "b" "c" "d" "e" "f"])))
