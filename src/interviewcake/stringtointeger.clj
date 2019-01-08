(ns interviewcake.stringtointeger)

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

(defn string->int [s]
  (let [n (count s)]
    (->> s
         (map vector (range))
         (map (fn [[i c]] (* (Math/pow 10 (- n i 1)) (char->int c))))
         (reduce +)
         int)))

(assert (= 0 (string->int "")))
(assert (= 0 (string->int "0")))
(assert (= 1 (string->int "1")))
(assert (= 2 (string->int "2")))
(assert (= 10 (string->int "10")))
(assert (= 101 (string->int "101")))
