(ns interviewcake.topscore)

(defn counting-sort
  "Returns a sorted xs in O(n) time and O(n) space
   xs: unsorted items - [int]
   l: highest possible value that x in xs can take - int"
  [xs l]
  (let [sort-vec (into [] (repeat l nil))
        counts (reduce (fn [acc x] (update acc x (fnil inc 0))) sort-vec xs)
        reducer-fn (fn [acc [i x]]
                     (if-not x
                       acc
                       (into acc (repeat x i))))]
    (reduce reducer-fn [] (map vector (range) counts))))

(assert (= (range 100)
           (counting-sort (shuffle (range 100)) 100)))
