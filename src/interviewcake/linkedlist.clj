(ns interviewcake.linkedlist)

;; ---------------
;; Linked List API
;; ---------------

(defn node [value]
  {:value value :next nil})

(defn insert [xs value]
  (assoc (node value) :next xs))

(defn uncons [{:keys [value next] :as l}]
  [value next])

(defn coll->list [coll]
  (reduce insert nil (reverse coll)))

(defn list->coll [ls]
  (if (nil? ls)
    ls
    (let [[value ks] (uncons ls)]
      (into [value] (list->coll ks)))))

(assert (= [1 2 3] (list->coll (coll->list [1 2 3]))))

;; ---------------
;; Deleting a Node
;; ---------------

(defn delete-node-aux [acc {:keys [value next] :as ls} n]
  (cond
    (nil? ls)                        (coll->list acc)
    (and (= n ls) (nil? next))       (coll->list acc)
    (and (= n ls) (not (nil? next))) (recur acc next n)
    :else                            (recur (conj acc value) next n)))

(defn delete-node [ls n]
  (delete-node-aux [] ls n))

(def linked-list
  (coll->list (range 10)))

(def pointer-2
  (-> linked-list :next :next))

(def pointer-4
  (-> linked-list :next :next :next :next))

(assert (-> (delete-node linked-list pointer-2)
             list->coll
             set
             (get 2)
             nil?))

(assert (-> (delete-node linked-list pointer-4)
             list->coll
             set
             (get 4)
             nil?))

;; ------------------
;; Cyclic linked list
;; ------------------

(def cyclic-ll
  (let [linear-list (coll->list (range 4))
        pointer-1 (:next linear-list)]
    (assoc-in linear-list [:next :next :next :next]
              (:next cyclic-ll))))

(defn cyclic? [slow fast]
  ;; Debugging
  ; (println "slow-> " (:value slow))
  ; (println "fast-> " (:value fast))

  (cond
    (nil? fast) false
    (nil? slow) false
    :else
    (or (= (:value slow) (:value fast))
        (cyclic? (:next slow) (:next (:next fast))))))

(cyclic? cyclic-ll (:next cyclic-ll))

; -------------------
; Reverse linked list
; -------------------

(defn reverse-ll-aux [acc {:keys [value next] :as ll}]
  (cond
    (nil? ll) (coll->list acc)
    :else (recur (cons value acc) next)))

(defn reverse-ll [ll]
  (reverse-ll-aux () ll))

(-> 10
    range
    coll->list
    reverse-ll)
