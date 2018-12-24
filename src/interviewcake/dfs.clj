(ns interviewcake.dfs)

;; DFS
;; * Requires less memory than BFS
;; * Can be implemented using tail callled recursion
;; * Does not find the shortest path between two nodes

(def graph
  {1 [2 3 4]
   2 [5]
   3 [6 7]
   4 [8]
   5 [9]
   6 [10]})

(defn dfs
  [graph node]
  (loop [stack [node]
         visited #{}]
    (if (not (seq stack))
      visited
      (let [s (peek stack)
            new-visited (conj visited s)
            next-nodes (remove new-visited (get graph s))
            new-stack (apply conj (pop stack) next-nodes)]
        (println "s: " s)
        (println "new-visited: " new-visited)
        (println "new-stack " new-stack)
        (recur new-stack new-visited)))))

(dfs graph 1)

(defn dfs-recursive-aux
  [stack visited graph]
  (if (not (seq stack))
    visited
    (let [s (peek stack)
          new-visited (conj visited s)
          next-nodes (remove new-visited (get graph s))
          new-stack (apply conj (pop stack) next-nodes)]
      (println "s: " s)
      (println "new-visited: " new-visited)
      (println "new-stack " new-stack)
      (recur new-stack new-visited graph))))

(dfs-recursive-aux [1] #{} graph)

(defn dfs-recursive
  [graph s]
  (dfs-recursive-aux [s] #{} graph))

(dfs-recursive graph 1)
