(ns interviewcake.rectangularlove)

(def r1 {:left-x 1
         :bottom-y 1
         :width 6
         :height 3})

(def r2 {:left-x 5
         :bottom-y 2
         :width 3
         :height 6})

(def r3 {:left-x 5
         :bottom-y -4
         :width 3
         :height 6})

(def r4 {:left-x 3
         :bottom-y -4
         :width 3
         :height 6})

(defn intersect?
  [[x1 x2 y1 y2] [x3 x4 y3 y4]]
  (and (or (< x1 x3 x2)
           (< x3 x1 x4))
       (or (< y1 y3 y2)
           (< y3 y1 y4))))

(defn intersection
  [r1 r2]
  (let [x1 (:left-x r1)
        x2 (+ x1 (:width r1))
        y1 (:bottom-y r1)
        y2 (+ y1 (:height r1))

        x3 (:left-x r2)
        x4 (+ x3 (:width r2))
        y3 (:bottom-y r2)
        y4 (+ y3 (:height r2))]
    ;; Debugging
    (cond
      (> x1 x3) (intersection r2 r1)
      (not (intersect? [x1 x2 y1 y2] [x3 x4 y3 y4])) nil
      (>= y1 y3) {:left-x x3
                  :bottom-y y1
                  :width (- x2 x3)
                  :height (- y4 y1)}

      :else {:left-x x3
             :bottom-y y3
             :width (- x2 x3)
             :height (- y2 y3)})))

(assert (= (intersection r1 r2)
           (intersection r2 r1)))
(assert (= (intersection r1 r3)
           (intersection r3 r1)))

(intersection r1 r4)

(defn axis-intersection
  [x1 x2 x3 x4]
  {:pre [(< x3 x4) (< x1 x2)]
   :post [(or (nil? %) (<= (first %) (second %)))]}
  (cond
    (< x3 x1) (axis-intersection x3 x4 x1 x2)
    ;; Can assume the invariant: x3 >= x1
    (<= x1 x3 x2) [x3 (min x2 x4)]
    :else nil))

(assert (= [2 4] (axis-intersection 1 5 2 4)))
(assert (= (axis-intersection 2 4 1 5)
           (axis-intersection 1 5 2 4)))
(assert (= [2 5] (axis-intersection 1 5 2 6)))
(assert (= (axis-intersection 2 6 1 5)
           (axis-intersection 1 5 2 6)))
(assert (= nil (axis-intersection 1 5 6 7)))
(assert (= [5 5] (axis-intersection 1 5 5 7)))

(defn rect-intersection
  [r1 r2]
  (let [x1 (:left-x r1)
        x2 (+ x1 (:width r1))
        y1 (:bottom-y r1)
        y2 (+ y1 (:height r1))

        x3 (:left-x r2)
        x4 (+ x3 (:width r2))
        y3 (:bottom-y r2)
        y4 (+ y3 (:height r2))

        [u v :as x] (axis-intersection x1 x2 x3 x4)
        [t s :as y] (axis-intersection y1 y2 y3 y4)]
    (if (or (not x) (not y))
      nil
      {:left-x u
       :bottom-y t
       :width (- v u)
       :height (- s t)})))

(rect-intersection r1 r2)
(rect-intersection r1 r3)
(rect-intersection r1 r4)
