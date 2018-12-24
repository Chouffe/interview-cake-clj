(ns interviewcake.coins)

(defn solve
  "returns the number of ways to make the `amount` money with coins of the available `denomination`
  `amount` is an integer
  `denomination` is a set of integers"
  [denominations amount]
  (cond
    (= amount 0)  1
    (neg? amount) 0
    ; (get denominations amount) (+ 1 (solve (disj denominations amount) amount))
    :else
    (->> denominations
         (map (fn [denomination] (- amount denomination)))
         (remove neg?)
         (map (partial solve denominations))

         (reduce +))))


(solve #{1} 2)
(solve #{1 2} 3)
(solve #{1 2 3} 3)
(solve #{1 2 3} 4)
