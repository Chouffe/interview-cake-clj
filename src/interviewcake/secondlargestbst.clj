(ns interviewcake.secondlargestbst)

(def bst-tree
  {:value 5
   :left {:value 3
          :left {:value 2
                 :left nil
                 :right nil}
          :right {:value 4
                 :left nil
                 :right nil}}
   :right  {:value 7
            :left {:value 6
                   :right nil
                   :left nil}
            :right {:value 10
                    :left {:value 8
                           :right nil
                           :left nil}
                    :right {:value 11
                            :right nil
                            :left {:value 10.5}}}}})
(defn largest
  "finds largest element in O(n) = depth of the tree"
  [{:keys [left right value] :as tree}]
  (if-not right
    value
    (largest right)))

(defn second-largest
  "finds second largest element in bst tree in O(log n) time"
  [{:keys [left right value] :as tree}]
  (cond
    (and (not right) (:right left))       (largest (:right left))
    (and (not right) (not (:right left))) (:value left)
    :else                                 (or (second-largest right) value)))

(assert (= 11 (largest bst-tree)))
(assert (= 10.5 (second-largest bst-tree)))
