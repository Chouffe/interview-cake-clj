(ns interviewcake.lowestcommonancestor)

(def tree
  {:value :a
   :left {:value :b
          :left {:value :d
                 :left {:value :h}
                 :right {:value :i}}
          :right {:value :e
                  :left {:value :j}
                  :right {:value :k}}}
   :right {:value :c
           :left {:value :f}
           :right {:value :g}}})

(defn tree-search
  [{:keys [left right value] :as tree} e]
  (cond
    (nil? tree) false
    (= e value) true
    :else (or (tree-search left e)
              (tree-search right e))))

(tree-search tree :j)
(tree-search tree :z)

(defn lowest-common-ancestor
  [{:keys [left right value] :as tree} node1 node2]
  (let [l1 (tree-search left node1)
        l2 (tree-search left node2)
        r1 (tree-search right node1)
        r2 (tree-search right node2)]
    (cond
      (or (and l1 r2) (and l2 r1)) tree
      (and l1 l2) (recur left node1 node2)
      (and r1 r2) (recur right node1 node2)
      :else :not-reachable)))

(lowest-common-ancestor tree :j :d)
