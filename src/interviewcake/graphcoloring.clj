(ns interviewcake.graphcoloring
  (:require [clojure.set :as s]))

(def graph
  {:a {:neighbors [:b :c :d]}
   :b {:neighbors [:a :d]}
   :c {:neighbors [:a]}
   :d {:neighbors [:a :b]}})

(def colored-graph
  {:a {:neighbors [:b :c :d] :color 0}
   :b {:neighbors [:a :d] :color 1}
   :c {:neighbors [:a] :color 2}
   :d {:neighbors [:a :b] :color 3}})

(defn color-graph
  "colours a graph and returns a colored graph"
  [graph colors]

  ;; TODO: turn it into a reduce operation instead
  (loop [nodes (into [] (keys graph))
         colored-graph graph]
    (if-not (seq nodes)
      colored-graph
      (let [node (peek nodes)
            neighbors (->> graph node :neighbors)
            illegal-colors (->> neighbors (map (fn [node] (:color (get colored-graph node)))) (remove nil?) set)
            node-color (first (s/difference colors illegal-colors))
            new-colored-graph (assoc-in colored-graph [node :color] node-color)
            new-nodes (pop nodes)]

        ;; Debugging
        ; (println "node: " node)
        ; (println "colored-graph: " colored-graph)
        ; (println "neighbors: " neighbors)
        ; (println "illegal-colors: " illegal-colors)
        ; (println "node-color: " node-color)
        ; (println "new-nodes " new-nodes)
        ; (println "new-colored-graph: " new-colored-graph)

        (recur new-nodes new-colored-graph)))))

(color-graph graph #{:blue :red :green})
