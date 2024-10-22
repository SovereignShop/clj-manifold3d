(ns clj-manifold3d.utils)

(defmacro cos [x] `(Math/cos ~x))
(defmacro acos [x] `(Math/acos ~x))
(defmacro sin [x] `(Math/sin ~x))
(defmacro asin [x] `(Math/asin ~x))
(defmacro tan [x] `(Math/tan ~x))
(defmacro atan [x] `(Math/atan ~x))
(defmacro sqr [x] `(Math/pow ~x 2))
(defmacro sqrt [x] `(Math/sqrt ~x))
(defmacro pow [x exp] `(Math/pow ~x ~exp))

(defmacro |2 [arg] `(/ ~arg 2))
(defmacro |3 [arg] `(/ ~arg 3))
(defmacro |4 [arg]  `(/ ~arg 4))

(def pi Math/PI)
(def pi|2 (/ pi 2))
(def pi|3 (/ pi 3))
(def pi|4 (/ pi 4))
(def pi|5 (/ pi 5))
(def pi|6 (/ pi 6))
(def two-pi (* 2 pi))

(def TT 360)
(def T 180)
(def T|2 180/2)
(def T|3 180/3)
(def T|4 180/4)
(def T|5 180/5)
(def T|6 180/6)

(defn abc->ABC
  "side,side,side -> angle,angle,angle using Law of Cosines."
  [a b c]
  (let [C (Math/acos (/ (- (Math/pow c 2)
                           (Math/pow a 2)
                           (Math/pow b 2))
                        (- (* 2 a b))))
        B (Math/acos (/ (- (Math/pow b 2)
                           (Math/pow a 2)
                           (Math/pow c 2))
                        (- (* 2 a c))))
        A (- Math/PI (+ C B))]
    [A B C]))

(defn nearest-multiple [x m]
  (* (quot x m) m))

(defn vec-add
  [v1 v2]
  (with-meta
    [(+ (first v1) (first v2)) (+ (second v1) (second v2))]
    (meta v1)))

(defn vec-sub
  [v1 v2]
  (with-meta
    [(- (first v1) (first v2)) (- (second v1) (second v2))]
    (meta v1)))

(defn vec-scale
  [v scalar]
  (with-meta
    [(* (first v) scalar) (* (second v) scalar)]
    (meta v)))

(defn vec-length
  [v]
  (Math/sqrt (+ (Math/pow (first v) 2) (Math/pow (second v) 2))))

(defn vec-magnitude [v]
  (Math/sqrt (reduce + (map * v v))))

(defn vec-normalize [v]
  (let [magnitude (vec-magnitude v)]
    (with-meta (mapv #(/ % magnitude) v)
      (meta v))))

(defn vec-dot
  [v1 v2]
  (+ (* (first v1) (first v2)) (* (second v1) (second v2))))

(defn angle-between-vectors [v1 v2]
  (let [dot-product (vec-dot v1 v2)
        magnitudes (* (vec-length v1) (vec-length v2))]
    (Math/acos (min 1 (max -1 (/ dot-product magnitudes))))))

(defn angle-between-vectors-cw [v1 v2]
  (let [dot-product (vec-dot v1 v2)
        cross-product (- (* (v1 0) (v2 1)) (* (v1 1) (v2 0)))
        magnitudes (* (vec-length v1) (vec-length v2))
        angle (Math/acos (min 1 (max -1 (/ dot-product magnitudes))))]
    (if (neg? cross-product)
      angle
      (- (* 2 Math/PI) angle))))

(defn angle-between-vectors-ccw [v1 v2]
  (let [dot-product (vec-dot v1 v2)
        cross-product (- (* (v1 0) (v2 1)) (* (v1 1) (v2 0)))
        magnitudes (* (vec-length v1) (vec-length v2))
        angle (Math/acos (min 1 (max -1 (/ dot-product magnitudes))))]
    (if (pos? cross-product)
      angle
      (- (* 2 Math/PI) angle))))

(defn cross-product-z
  [v1 v2]
  (- (* (nth v1 0) (nth v2 1)) (* (nth v1 1) (nth v2 0))))
