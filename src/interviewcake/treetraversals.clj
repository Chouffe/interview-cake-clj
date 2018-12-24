(ns interviewcake.treetraversals)

(def tree
  {:value 1
   :left {:value 2
          :left {:value 4
                 :left nil
                 :right nil}
          :right {:value 5
                 :left nil
                 :right nil}}
   :right  {:value 3
            :left nil
            :right nil}})

;; Use of inorder traversal
;; In Binary Search Tree: it returns nodes in non-decreasing order
(defn inorder
  [{:keys [value left right] :as tree}]
  (if-not (or tree value)
    []
    (concat (inorder left) [value] (inorder right))))

(assert (= [4 2 5 1 3] (inorder tree)))

;; Use of preorder traversal
;; * Create a copy of the tree
;; * Get prefix expression of an expression tree (Polish Notation): https://en.wikipedia.org/wiki/Polish_notation
(defn preorder
  [{:keys [value left right] :as tree}]
  (if-not (or tree value)
    []
    (concat [value] (preorder left) (preorder right))))

(assert (= [1 2 4 5 3] (preorder tree)))

;; Use of postorder traversal
;; * Delete the tree
;; * Get the postfix notation of an expression tree: https://en.wikipedia.org/wiki/Reverse_Polish_notation
(defn postorder
  [{:keys [value left right] :as tree}]
  (if-not (or tree value)
    []
    (concat (postorder left) (postorder right) [value])))

(assert (= [4 5 2 3 1] (postorder tree)))
