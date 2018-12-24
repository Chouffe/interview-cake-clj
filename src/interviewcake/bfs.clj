(ns interviewcake.bfs)

;; Resource for visualizing the Graph: https://www.interviewcake.com/concept/python/bfs
;;
;; BFS:
;; * Finds the shortest path between starting and any other reachable node
;; * Generally requires more memory than DFS

; Encoded with adjacency list with a dictionary
(def graph
  {1 [2 3 4]
   2 [5]
   3 [6 7]
   4 [8]
   5 [9]
   6 [10]})

(defn bfs
  [graph node]
  (loop [queue [node]
         visited #{}]
    (if (not (seq queue))
      visited
      (let [s (first queue)
            new-visited (conj visited s)
            next-nodes (remove new-visited (get graph s))
            new-queue (apply conj (subvec queue 1) next-nodes)]
        (println "s: " s)
        (println "new-visited: " new-visited)
        (println "new-queue: " new-queue)
        (recur new-queue new-visited)))))

(bfs graph 1)
