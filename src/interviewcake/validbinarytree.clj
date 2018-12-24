(ns interviewcake.validbinarytree)

(defn valid-binary-tree?
  [{:keys [left right value] :as tree}]
  (cond
    (not (or tree value))        true
    (and (not left) (not right)) true
    (and (not left) (:value right)) (and (>= (:value right) value)
                                         (valid-binary-tree? right))
    (and (not right) (:value left)) (and (<= (:value left) value)
                                         (valid-binary-tree? left))
    :else (and (<= (:value left) value)
               (>= (:value right) value)
               (valid-binary-tree? left)
               (valid-binary-tree? right))))

(assert (valid-binary-tree? nil))
(assert (valid-binary-tree? {:left nil :right nil :value 5}))
(assert (not (valid-binary-tree? {:left {:left nil :right nil :value 10} :right nil :value 5})))
(assert (valid-binary-tree? {:left {:left {:left nil :right nil :value 2} :right nil :value 4} :right nil :value 5}))
(assert (not (valid-binary-tree? {:left {:left {:left nil :right nil :value 6} :right nil :value 4} :right nil :value 5})))

;; Second approach: use DFS and keep track of lower-bound and upper-bound to check for validity
(defn valid-binary-tree-2-aux?
  [lower-bound upper-bound {:keys [value left right] :as tree}]
  (cond
    (or (not tree) (not value)) true

    (or (> value upper-bound)
        (< value lower-bound)) false

    :else (and (valid-binary-tree-2-aux? lower-bound value left)
               (valid-binary-tree-2-aux? value upper-bound right))))

(defn valid-binary-tree-2? [tree]
  (valid-binary-tree-2-aux?
    Integer/MIN_VALUE
    Integer/MAX_VALUE
    tree))

(assert (valid-binary-tree-2? nil))
(assert (valid-binary-tree-2? {:left nil :right nil :value 5}))
(assert (not (valid-binary-tree-2? {:left {:left nil :right nil :value 10} :right nil :value 5})))
(assert (valid-binary-tree-2? {:left {:left {:left nil :right nil :value 2} :right nil :value 4} :right nil :value 5}))
(assert (not (valid-binary-tree-2? {:left {:left {:left nil :right nil :value 6} :right nil :value 4} :right nil :value 5})))
