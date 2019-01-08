(ns interviewcake.core)

;; Graph Traversals

(def graph
  {1 [2]
   2 [3 4]
   3 [2 4]
   4 [3 2]})

(defn dfs-iterative
  [graph initial-node]
  (loop [visited #{}
         stack [initial-node]]
    (if (not (seq stack))
      visited
      (let [node (peek stack)
            next-nodes (remove visited (get graph node))
            new-visited (conj visited node)
            new-stack (apply conj (pop stack) next-nodes)]
        ; (println "node: " node)
        ; (println "stack " stack)
        ; (println "visited " visited)
        (recur new-visited new-stack)))))

(dfs-iterative graph 1)

(defn dfs-recursive
  [graph visited stack]
  ;; Debugging
  ; (println "stack: " stack)
  ; (println "visited: " visited)
  (if (not (seq stack))
    visited
    (let [node (peek stack)
          next-nodes (remove visited (get graph node))
          new-visited (conj visited node)
          new-stack (into (pop stack) next-nodes)]
      (recur graph new-visited new-stack))))

(dfs-recursive graph #{} [1])

(assert (= (dfs-recursive graph #{} [1])
           (dfs-iterative graph 1)))

(defn bfs
  [graph initial-node]
  (loop [visited #{}
         queue [initial-node]]
    (if (not (seq queue))
      visited
      (let [node (first queue)
            next-nodes (remove visited (get graph node))
            new-visited (conj visited node)
            new-queue (into (subvec queue 1) next-nodes)]
        ;; Debugging
        (println "node: " node)
        (println "next-nodes: " next-nodes)
        (println "visited: " visited)
        (println "queue: " queue)
        (recur new-visited new-queue)))))

(bfs graph 1)
