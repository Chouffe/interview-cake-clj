(ns interviewcake.isbalanced)


; Question: Write a function `balanced?` that checks whether a string `s` is balanced
    ; - Input: string "( [ ] { } ( ) )" (potential whitespaces)
    ; - Output: boolean

(def matches
  {\} \{
   \) \(
   \] \[})

(defn balanced-aux?
  [stack [s0 & ss :as s]]
  (cond
    (not (seq s)) (not (seq stack))
    (#{\( \[ \{} s0) (balanced-aux? (conj stack s0) ss)
    (get (set (keys matches)) s0) (and (= (get matches s0) (peek stack))
                                       (balanced-aux? (pop stack) ss))
    :else (balanced-aux? stack ss)))

(assert (balanced-aux? [] ""))
(assert (balanced-aux? [] "[]"))
(assert (not (balanced-aux? [] "[[")))
(assert (not (balanced-aux? [] "[}")))
(assert (balanced-aux? [] "({})"))

(defn balanced? [s]
  (balanced-aux? [] s))

;; Add ruby ||

(defn balanced-aux-2?
  [stack [s0 & ss :as s]]
  (cond
    (not (seq s)) (not (seq stack))
    (#{\( \[ \{} s0) (balanced-aux-2? (conj stack s0) ss)
    (= \| s0) (or (balanced-aux-2? (conj stack s0) ss)
                  (and (= \| (peek stack))
                       (balanced-aux-2? (pop stack) ss)) )
    (get (set (keys matches)) s0) (and (= (get matches s0) (peek stack))
                                       (balanced-aux-2? (pop stack) ss))
    :else (balanced-aux-2? stack ss)))

(assert (balanced-aux-2? [] "({})"))
(assert (balanced-aux-2? [] "|({})|"))
(assert (balanced-aux-2? [] "|||({})|"))
