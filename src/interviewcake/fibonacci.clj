(ns interviewcake.fibonacci)

;; Resource: http://sercanulucan.com/blog/2017/01/four-ways-of-fibn-with-clojure/

(defn fibo1
  "Naive implementation of fibo. O(2^n) and O(n) space: stack frames"
  [n]
  (if (<= n 1)
    n
    (+ (fibo1 (dec n))
       (fibo1 (dec (dec n))))))

(map fibo1 (range 10))

(defn fibo2
  "Memoized version of fibo with an array. O(n) time and O(n) in space"
  [n]
  (if (<= n 1)
    n
    (->> (range n)
         (reduce (fn [[x0 x1 & _ :as xs] _]
                   (conj xs (+ x0 x1)))
                 (list 1 0))
         first)))

(defn memo-fibo-aux
  [cache n]
  (if (get cache n)
    [cache (get cache n)]
    (let [[new-cache fibo-2] (memo-fibo-aux cache (- n 2))
          [new-new-cache fibo-1] (memo-fibo-aux new-cache (dec n))
          result (+ fibo-1 fibo-2)]
      [(assoc new-new-cache n result) result])))

(defn memo-fibo
  "Memoized version of fibo using a dictionary for caching"
  [n]
  (last (memo-fibo-aux {0 0 1 1} n)))

(defn my-memoize [f]
  (let [mem (atom {})]
    (fn [& args]
      (if-let [e (get @mem args)]
        e
        (let [ret (apply f args)]
          (swap! mem assoc args ret)
          ret)))))

(map fibo1 (range 10))
(map fibo2 (range 10))
(map memo-fibo (range 10))

(defn fib [n]
  (if (<= n 1)
    n
    (+ (fib (dec n)) (fib (dec (dec n))))))

(time (fib 36))
;"Elapsed time: 5040.546718 msecs"
; 14930352

;; Proper way to use `memoize`
(def fib-m
  (memoize
    (fn [n]
      (if (<= n 1)
        n
        (+ (fib-m (dec n)) (fib-m (dec (dec n))))))))

(time (fib-m 36)) ; can stack overflow
;"Elapsed time: 0.507994 msecs"
; 14930352

(defn fib-recursive
  [n]
  (if (<= n 1)
    n
    (loop [x 1 f0 0 f1 1]
      (if (>= x n)
        f1
        (recur (inc x) f1 (+ f0 f1))))))

(map fib-recursive (range 10))

;; (fib-recursive 100)  ;; Integer Overflow
;; Use +' to use BigInt
