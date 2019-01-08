(ns interviewcake.mnemonics)

; Question: Print all possible words from phone digits
; https://www.geeksforgeeks.org/find-possible-words-phone-digits/

(defn char->int [c]
  (case c
    \0 0
    \1 1
    \2 2
    \3 3
    \4 4
    \5 5
    \6 6
    \7 7
    \8 8
    \9 9))

(def mnemonics
  {2 [\A \B \C]
   3 [\D \E \F]
   4 [\G \H \I]
   5 [\J \K \L]
   6 [\M \N \O]
   7 [\P \Q \R \S]
   8 [\T \U \V]
   9 [\W \X \Y \Z]})

(defn digit->mnemonics [d]
  (->> d str (map char->int) (map mnemonics)))

(assert (= 3 (count (digit->mnemonics 234))))

(defn combinations
  [[y & ys :as xs]]
  (cond
    (not (seq xs)) []
    (not (seq ys)) (mapv (fn [x] [x]) y)
    :else (let [css (combinations ys)]
            (for [e y cs css]
              (cons e cs)))))

(combinations [[:a :b]])
(combinations [[:a :b] [:c :d]])
(combinations [[:a :b] [:c :d] [:e :f]])

(defn solve [d]
  (combinations (digit->mnemonics d)))

(assert (= 9 (count (solve 23))))
