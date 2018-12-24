(ns interviewcake.singlerifflecheck)

(def deck (shuffle (range 52)))

(def h1 (take 26 deck))
(def h2 (drop 26 deck))

(assert (= (count h1) (count h2)))

(def shuffled-deck (concat h1 h2))

(defn single-riff?
  "invariant: same order in shuffle-deck for h1 and h2
  complexity:
    * time O(n)
    * space O(n): allocating two sets"
  [shuffled-deck h1 h2]
  (and (= h2 (remove (set h1) shuffled-deck))
         (= h1 (remove (set h2) shuffled-deck))))

(defn single-riff2?
  "invariant t1 or t2 is on top of shuffle-deck - recur from here
  complexity:
    * time O(n)
    * space O(1) : TCO with recursion"
  [[t & r :as shuffle-deck] [t1 & r1 :as h1] [t2 & r2 :as h2]]
  (cond
    (not (seq shuffle-deck)) true
    (= t1 t)                 (recur r r1 h2)
    (= t2 t)                 (recur r h1 r2)
    :else                     false))

(assert (single-riff? shuffled-deck h1 h2))
(assert (single-riff2? shuffled-deck h1 h2))
