(ns interviewcake.meshmessage)

(def graph
  {
   "Min"     ["William" "Jayden" "Omar"]
   "William" ["Min" "Noam"]
   "Jayden"  ["Min" "Amelia" "Ren" "Noam"]
   "Ren"     ["Jayden" "Omar"]
   "Amelia"  ["Jayden" "Adam" "Miguel"]
   "Adam"    ["Amelia" "Miguel" "Sofia" "Lucas"]
   "Miguel"  ["Amelia" "Adam" "Liam" "Nathan"]
   "Noam"    ["Nathan" "Jayden" "William"]
   "Omar"    ["Ren" "Min" "Scott"]
   })

;; Question: find the shortest route for a message from one user  (the sender) to another  (the recipient). Return a list of users that make up this route.


;; Use a BFS to find the shortest path in O (n)


(defn bfs
  "assumes sender and recipient are connected in the graph: there exists a path from sender to recipient"
  [graph sender recipient]
  (loop [queue [[sender []]] ;; [[node current-path]
         visited #{}]
    (let [[next-node current-path] (first queue)
          next-nodes (get graph next-node)
          new-visited (conj visited next-node)
          new-queue (apply conj (subvec queue 1)
                           (map (fn [node] [node (conj current-path next-node)])
                                (remove visited next-nodes)))]
      ;; Debugging
      ; (println "next-node: " next-node)
      ; (println "next-nodes: " next-nodes)
      ; (println "new-visited: " new-visited)
      ; (println "new-queue: " new-queue)

      ;; TODO: add a condition if queue is empty
      (cond
        (not (seq queue)) nil

        (= next-node recipient) [new-visited (conj current-path recipient)]

        :else (recur new-queue new-visited)))))

(defn shortest-route
  [graph sender recipient]
  (if-let [[visited path] (bfs graph sender recipient)]
    path
    "Route not found"))

(shortest-route graph "Jayden" "Adam")
