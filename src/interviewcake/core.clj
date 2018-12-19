(ns interviewcake.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(doseq [x [-1 0 1]
        y [1 2 3]]
  (prn (* x y)))

(for [x [-1 0 1]
      y [1 2 3]]
  (* x y))

(partition 3 2 [1 2 3 4 5 6 7 8])
(distinct [1 2 3 4 3 2 1])
(split-at 3 [1 2 3 4 5 6])

(partition 3 3 nil (range 11))
(interpose "-" [1 2 3])
(interleave [1 2 3] (repeat "-"))
(reduce-kv (fn [m k v]
             (println m)
             (println k)
             (println v)
             (assoc m k (inc v))) {}
           (zipmap [:a :b :c :d] (range 10)))

(zipmap [:a :b :c :d] (range 10))

(conj [1 2 3] 1)
(conj (list 1 2 3) 1)
(conj #{1 2 3} 4)
(conj {:a 1} [:b 2])
(peek [1 2 3])
(peek (list 1 2 3 ))
(pop (list 1 2 3))
(conj (list 1 2 3) 4)

(cond-> 1
  true inc
  false (* 42)
  :always (or (str "hello")))

(subs "Clojure" 1)
