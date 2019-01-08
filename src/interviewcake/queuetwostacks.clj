(ns interviewcake.queuetwostacks)

(defn enqueue
  "enqueues `e` into `queue`"
  [[s1 s2 :as queue] e]
  [(conj s1 e) s2])

(defn re-stack
  "re order elements from the queue"
  [[s1 s2 :as queue]]
  (if-not (seq s1)
    [s1 s2]
    (recur [(pop s1) (conj s2 (peek s1))])))

(defn dequeue
  "dequeues from queue, returns the new queue and the element that is dequeued"
  [[s1 s2 :as queue]]
  (if (seq s2)
    [[s1 (pop s2)] (peek s2)]
    (dequeue (re-stack [s1 s2]))))

(-> [[] []]
     (enqueue 1)
     (enqueue 2)
     (enqueue 3)
     (enqueue 4)
     dequeue
     first
     dequeue)
