(ns interviewcake.powerset)

(defn power-set
  "returns power-set of set `coll`"
  [coll]
  (if-not (seq coll)
    #{#{}}
    (let [e (first coll)
          pss (power-set (disj coll e))]
      (into pss (map (fn [s] (conj s e)) pss)))))


(power-set #{})
(power-set #{:a})
(power-set #{:a :b})
(power-set #{:a :b :c})
(power-set #{:a :b :c :d})
