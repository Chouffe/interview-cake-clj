(ns interviewcake.reversewords)

(defn reverse-words
  "reverse words separated by space char"
  [xs]
  (->> (partition-by (partial = \space) xs)
       reverse
       flatten))

(def message (map char "cake pound steal") )

(assert (= '(\s \t \e \a \l \space \p \o \u \n \d \space \c \a \k \e)
           (reverse-words message)))
