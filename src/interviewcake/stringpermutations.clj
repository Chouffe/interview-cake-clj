(ns interviewcake.stringpermutations)

(defn permutations [s]
  (cond
    (not (seq s))   #{}
    (= (count s) 1) #{s}
    :else (let [s0 (subs s 0 1)
                xs (permutations (subs s 1))]
            (into #{}
                  (for [i (range (count s)) perm xs]
                    (str (subs perm 0 i) s0 (subs perm i)))))))

(permutations "abc")
(permutations "abcdef")
