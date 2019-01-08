(ns interviewcake.parenthesismatching)

;; TODO: change dec to inc
(defn solve-aux [stack position current-position s]

  ;; Debugging
  ; (println "stack: " stack)
  ; (println "current-position: " current-position)
  ; (println "position: " position)
  ; (println "s: " s)

  (cond
    (pos? current-position)      (recur stack position (dec current-position) (subs s 1))
    (zero? current-position)     (recur (conj stack (first s)) position (dec current-position) (subs s 1))
    (not (seq stack))            (dec (- position current-position))
    (not (seq s))                (throw (Throwable. "Could not find matching parens"))
    (= (first s) \()             (recur (conj stack (first s)) position (dec current-position) (subs s 1))

    (and (= (first s) \))
         (= (peek stack) \())    (recur (pop stack) position (dec current-position) (subs s 1))

    (and (= (first s) \))
         (not= (peek stack) \()) (throw (Throwable. "Unbalanced string"))

    :else                        (recur stack position (dec current-position) (subs s 1))))

(defn solve [position s]
  (solve-aux [] position position s))

(solve 0 "()")
(solve 0 "(hello world)")

(def sentence
  "Sometimes (when I nest them (my parentheticals) too much (like this (and this))) they get confusing.")

(solve 10 sentence)


(defn solve-2-aux [position parens-count s]
  ;; Debugging
  ; (println "position: " position)
  ; (println "parens-count: " parens-count)
  ; (println "s: " s)

  (cond
    (zero? parens-count) position
    (not (seq s)) (throw (Throwable. "No closing parens..."))
    (= (first s) \() (recur (inc position) (inc parens-count) (subs s 1))
    (= (first s) \)) (recur (inc position) (dec parens-count) (subs s 1))
    :else (recur (inc position) parens-count (subs s 1))
    ))

(defn solve-2 [position s]
  (solve-2-aux position 1 (subs s (inc position))))

(solve-2 0 "(hello world)")
(solve-2 10 sentence)
