(ns interviewcake.ipstrings
  (:require [clojure.string :as s]))

; Question: Given a string  "19216811", outputs all the possible IPv4 addresses this string can be (9 possibilities here)

(defn combine [s xss]
  (mapv (fn [xs] (conj xs s)) xss))

(defn all-blx-aux [s]
  (let [n (count s)]
    (cond
      (= n 0) [[]]
      (= n 1) [[(Integer/parseInt s)]]
      :else
      (let [cs (combine (Integer/parseInt (subs s 0 1))
                        (all-blx-aux (subs s 1)))]

        (cond-> cs
          (> n 1) (concat (combine (Integer/parseInt (subs s 0 2))
                                   (all-blx-aux (subs s 2))))
          (> n 2) (concat (combine (Integer/parseInt (subs s 0 3))
                                   (all-blx-aux (subs s 3)))))))))

(defn valid-block-range? [b]
  (and (>= b 0)
       (<= b 255)))

(defn ips [s]
  (let [all-blocks (all-blx-aux s)]
    (->> all-blocks
         (filter (fn [ip] (= 4 (count ip))))
         (filter (fn [ip] (every? valid-block-range? ip)))
         (map reverse)
         distinct
         )))

(combine "1" [["192"] ["19"] ["1"]])
(all-blx-aux "1")
(all-blx-aux "12")
(all-blx-aux "123")
(all-blx-aux "123456")

(ips "19216811")

;; Other solution

(defn ip-string->ip
  [ip-string dot1 dot2 dot3]
  (str (subs ip-string 0 dot1)
       "."
       (subs ip-string dot1 dot2)
       "."
       (subs ip-string dot2 dot3)
       "."
       (subs ip-string dot3)))

(defn valid-string?
  [string]
  (cond
    (not (seq string))            false
    (and (= \0 (first string))
         (> (count string) 1))    false
    :else (<= 1 (Integer. string) 255)))

(defn ip-string->ips
  [ip-string]
  (let [n (count ip-string)]
    (for [i (range 1 4)
          j (range (inc i) (+ (inc i) 3))
          k (range (inc j) (+ (inc j) 3))
          :when (and (< i j k n)
                     (valid-string? (subs ip-string 0 i))
                     (valid-string? (subs ip-string i j))
                     (valid-string? (subs ip-string j k))
                     (valid-string? (subs ip-string k)))]
      (ip-string->ip ip-string i j k))))

(ips "19216811")
(ip-string->ips "19216811")


(defn valid-binary-tree?
  [[range-min range-max] {:keys [left right value] :as tree}]
  ;; Debugging
  ; (println "tree: " tree)
  ; (println "range-min: " range-min)
  ; (println "range-max: " range-max)

  (if (or (nil? tree) (nil? value))
    true
    (and (<= range-min value range-max)
         (valid-binary-tree? [range-min (min value range-max)] left)
         (valid-binary-tree? [(max range-min value) range-max] right))))

(def btree {:value 5})
(def btree-2 {:value 5
              :left {:value 3}})
(def btree-3 {:value 5
              :left {:value 3
                     :left {:value 2}
                     :right {:value 4}}})
(def btree-4 {:value 5
              :left {:value 3
                     :left {:value 2}
                     :right {:value 4}}
              :right {:value 8
                      :left {:value 6}}})
(def btree-5 {:value 5
              :left {:value 3
                     :left {:value 2}
                     :right {:value 4}}
              :right {:value 8
                      :left {:value 6}
                      :right {:value 10
                              :left {:value 9}}}})
(def btree-6 {:value 5
              :left {:value 3
                     :left {:value 4}
                     :right {:value 2}}})

(assert (valid-binary-tree? [Integer/MIN_VALUE Integer/MAX_VALUE] btree))
(assert (valid-binary-tree? [Integer/MIN_VALUE Integer/MAX_VALUE] btree-2))
(assert (valid-binary-tree? [Integer/MIN_VALUE Integer/MAX_VALUE] btree-3))
(assert (valid-binary-tree? [Integer/MIN_VALUE Integer/MAX_VALUE] btree-4))
(assert (valid-binary-tree? [Integer/MIN_VALUE Integer/MAX_VALUE] btree-5))
(assert (not (valid-binary-tree? [Integer/MIN_VALUE Integer/MAX_VALUE] btree-6)))
