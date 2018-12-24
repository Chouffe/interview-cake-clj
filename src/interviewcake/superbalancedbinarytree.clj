(ns interviewcake.superbalancedbinarytree)

(def binary-tree
  {:left {:left {:left nil
                 :right nil
                 :value 6}
          :right nil
          :value 7}
   :right {:left {:left nil
                  :right nil
                  :value 1}
           :right nil
           :value 3}
   :value 5})

(defn superbalanced
  "returns [superbalanced? depth of tree"
  [{:keys [left right] :as tree}]
  (if-not tree
    [true 0]
    (let [[s1? d1] (superbalanced left)
          [s2? d2] (superbalanced right)]
      [(and s1? s2? (<= (Math/abs (- d2 d1)) 1))
       (inc (max d1 d2))])))

(Math/abs -1)

(superbalanced nil)
(superbalanced {:left nil :right nil :value 5})
(superbalanced {:left {:left nil
                       :right nil}
                :right nil
                :value 5})
(superbalanced {:left {:left {:left nil
                              :right nil}
                       :right nil}
                :right nil})
(superbalanced {:left {:left {:left nil
                              :right nil}
                       :right nil}
                :right {:left nil
                        :right nil}})

;; Can also be coded with a tree traversal DFS/BFS that stores the leaves with their depths
;; When we add to depths we check if there are > 3 different depths
;; Or if only 2, they are more than 2 apart

(def tree
  {1 [2 3]
   2 [4 5]
   3 [6 7]
   4 [8 9]})

(defn superbalanced-2
  "using DFS because we can use recursive version
  stack is a stack of [depth node]"
  [stack visited depths tree]

  ;; Debugging
  (println "stack: " stack)
  (println "visited: " visited)
  (println "depths: " depths)

  (cond
    (or (> (count depths) 2)
          (and (= 2 (count depths))
               (> (Math/abs (- (first depths) (second depths))) 1)))
    false
    (not (seq stack)) true

    :else
    (let [[s d] (peek stack)
          new-visited (conj visited s)
          next-nodes (remove visited (get tree s))
          new-stack (apply conj (pop stack) (map (fn [x] [x (inc d)]) next-nodes))
          new-depths (if-not (seq next-nodes) (conj depths d) depths)]
      (println "new-visited: " new-visited)
      (println "next-nodes: " next-nodes)
      (println "new-stack: " new-stack)
      (println "new-depths: " new-depths)
      (recur new-stack new-visited new-depths tree))))

(superbalanced-2 [[1 0]] #{} #{} {1 [2 3]
                                  2 [4 5]
                                  3 []
                                  4 []})

(first #{1 2 })
