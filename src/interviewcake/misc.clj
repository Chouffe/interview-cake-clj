(ns interviewcake.misc)

(defn m-concat [& xss]
  (reduce (fn [acc xs] (into acc xs)) [] xss))

(m-concat [1 2 3] [4 5 6])
(m-concat [1 2 3])
(m-concat)


;; inorder tree traversal
(defn m-flatten [x]
  (cond
    (not (sequential? x)) [x]
    (sequential? x) (apply concat (map m-flatten x))))

(m-flatten [1 [2] 3])

(flatten)

(seq 1)

(sequential? nil)
