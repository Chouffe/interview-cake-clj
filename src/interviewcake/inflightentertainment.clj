(ns interviewcake.inflightentertainment)

(defn naive-full-entertainment?
  "return true iff two numbers in movie-lengths is exactly equal to flight-length
  complexity:
    * time O(n^2)
    * space O(n)"
  [flight-length movie-lengths]
  (->> (for [[xi i] (map vector (range) movie-lengths)
                       [xj j] (map vector (range) movie-lengths)
                       :when (not= i j)]
                   (= flight-length (+ xi xj)))
       (some true?)
       boolean))

(defn full-entertainment?
  "return true iff two numbers in movie-lengths is exactly equal to flight-length
  complexity:
    * time O(n)
    * space O(n)"
  [flight-length movie-lengths]
  (let [xs (set movie-lengths)]
    (->> movie-lengths
         (map (fn [x] (get (disj xs x) (- flight-length x) false)))
         (some true?)
         boolean)))

;; Does not work when there are movies of same length
;; Could use a hashmap instead with key: movie length, value number of movies
;; Could use set with tuple (movie-index duration): nope because the lookup needs to be done with the duration only
(full-entertainment-v2? 10 [1 3 5 5 8])

(defn full-entertainment-v2?
  "return true iff two numbers in movie-lengths is exactly equal to flight-length
  complexity:
    * time O(n)
    * space O(n)"
  [flight-length movie-lengths]
  (let [movie-lengths-indexed (map vector movie-lengths (range))
        xs (frequencies movie-lengths)]
    (println movie-lengths-indexed)
    (println xs)
    (map (fn [x]))
    (->> movie-lengths-indexed
         (map (fn [x] ( (disj xs x) (- flight-length duration) false)))
         (some true?)
         boolean)))

;; Naive solution:
;; all pairs in movie-lengths and checks whether the sum is equal to flight-length
;; O (n^2)
(def flight-length 10)
(def movie-lengths [1 3 5 5 8])

(-> (frequencies movie-lengths)
     (update-in [6] (fnil dec 1)))

(def xs (frequencies movie-lengths))
(map (fn [x]
       (let [u (update xs x (fnil dec 1))
             w (- flight-length x)
             v (get u w 0)]
       (> v 0))) movie-lengths)

;; We can do better in O(n) using sets and iterate only once

(full-entertainment? 10 [1 3 5 8])
(full-entertainment-v2? 10 [1 3 5 5 8])
(naive-full-entertainment? 10 [1 3 5 8])

;; TODO: bonus questions
