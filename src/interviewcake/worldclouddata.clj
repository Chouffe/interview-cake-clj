(ns interviewcake.worldclouddata)

(def s1 "After beating the eggs, Dana read the next step:")
(def s2 "Add milk and eggs, then add flour and sugar.")

(defn clean-string [s]
  (-> s
      clojure.string/lower-case
      clojure.string/trim
      (clojure.string/replace #"!|:|\.|;|\?|," "")))

(defn world-cloud [s]
  "Returns frequencies of words in `s` to display as a world cloud"
  (-> s
      clean-string
      (clojure.string/split #" ")
      frequencies))

(world-cloud s2)

(defn m-frequencies
  "frequencies of elements in coll"
  [coll]
  (reduce (fn [acc x] (update acc x (fnil inc 0))) {} coll))

; (m-frequencies "hello world")

;; clojure.string functions

;; Reverse string in O(n)
; (clojure.string/reverse "Hello")

;; Split world on spaces
; (clojure.string/split "Hello world" #" ")

;; Join a collection of strings in O(n)
; (clojure.string/join " " ["Hello" "world"])

;; includes substr
; (clojure.string/includes? "hello world" "hello")

;; Replaces in string based on regex
; (clojure.string/replace "hello! World:" #"!|:" " ")

