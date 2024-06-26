(ns clj-manifold3d.core-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure.java.io :as io]
   [clj-manifold3d.core :refer [mesh cube get-mesh manifold get-properties mirror union scale
                                compose decompose translate get-mesh-gl get-mesh import-mesh loft
                                difference smooth sphere refine cylinder polyhedron export-mesh
                                tetrahedron circle frame rotate square]]))

(defn- glm-to-vectors [v]
  (for [i (range (.size v))]
    (let [v (.get v i)]
      [(.x v) (.y v) (.z v)])))

(defn about=
  ([x y]
   (about= x y 0.05))
  ([x y eps]
   (< (Math/abs (- x y)) eps)))

(defmacro test-props [props m]
  `(let [e-props# (get-properties ~m)]
    (doseq [[k# x#] e-props#]
      (let [y# (get ~props k#)]
        (is (about= (double x#) (double y#)))))))

(deftest test-mesh
  (let [m (mesh
           :tri-verts [[2 0 1] [0 5 1]
                       [0 2 6] [6 2 3]
                       [2 1 3] [1 7 3]
                       [0 4 5] [0 6 4]
                       [1 5 7] [4 7 5]
                       [4 6 7] [6 3 7]]
           :vert-pos [[0.0 0.0 0.0]
                      [0.0 0.0 10.0]
                      [0.0 10.0 0.0]
                      [0.0 10.0 10.0]
                      [10.0 0.0 0.0]
                      [10.0 0.0 10.0]
                      [10.0 10.0 0.0]
                      [10.0 10.0 10.0]])]
    (is (= (get-properties (manifold m))
           (get-properties (cube 10 10 10 false))))))

(deftest test-polyhedron
  (let [hedron (polyhedron [[0 0 0]
                            [5 0 0]
                            [5 5 0]
                            [0 5 0]
                            [0 0 5]
                            [5 0 5]
                            [5 5 5]
                            [0 5 5]]
                           [[0 3 2 1]
                            [4 5 6 7]
                            [0 1 5 4]
                            [1 2 6 5]
                            [2 3 7 6]
                            [3 0 4 7]])
        v (cube 5 5 5 false)]
    (is (= 12 (-> hedron get-mesh .triVerts .size)))
    (is (about= (:volume (get-properties hedron))
                (:volume (get-properties v))))
    (is (about= (:surface-area (get-properties hedron))
                (:surface-area (get-properties v))))))

(deftest test-tetrahedron
  (let [tetra (tetrahedron)
        m (get-mesh tetra)]
    (is (= (-> m .triVerts .size) 4))
    (about= 2.66 (-> tetra get-properties :volume))))

(deftest test-cube
  (let [c (cube 10 10 10 false)
        p (get-properties c)]
    (is (about= (:volume p) (* 10 10 10)))
    (is (about= (:surface-area p) (* 10 10 6)))))

(deftest test-cylinder
  (let [r 5
        h 10
        c (cylinder h r r 500)
        p (get-properties c)]
    ;; Note volume is dependent on number of circular segments.
    (is (about= (:volume p) (* Math/PI (Math/pow r 2) h) 0.2))))

(deftest test-mirror
  (let [c (cube 10 10 10 false)
        m (mirror c [0 1 0])
        verts (.vertPos (get-mesh m))]
    (doseq [i (range (.size verts))]
      (is (not (pos? (.y (.get verts i))))))))

(deftest test-smooth-refine
  (let [tet (tetrahedron)
        smoothed (-> tet
                     (get-mesh)
                     (smooth)
                     (refine 100))
        props (get-properties smoothed)]
    (is (about= (:volume props) 17.38 0.1))
    (is (about= (:surface-area props) 33.38 0.1 ))))

(deftest test-compose
  (let [man (compose [(scale (tetrahedron) [5 5 5])
                      (-> (cube 1 1 1 false) (translate [0 0 10]))
                      (-> (sphere 1 4) (translate [0 0 20]))])]
    (is (about=
         (transduce (map (comp :volume get-properties)) + (decompose man))
         (:volume (get-properties man))))))

(deftest test-scale
  (let [factor 2
        w 10
        c (cube w w w)
        scaled (scale c [factor factor factor])
        scaled-props (get-properties scaled)]
    (is (about= (:volume (get-properties (cube (* w factor) (* w factor) (* w factor))))
                (:volume scaled-props)))))

(deftest test-get-mesh-gl
  (let [w 20
        m (cube w w w)
        test-file-name  "test/data/cube-gl-mesh.glb"
        mesh (get-mesh-gl m [0 1 2])
        _  (get-mesh-gl m)]
    (try
      (export-mesh mesh test-file-name)
      (is
       (about=
        (* w w w)
        (-> (import-mesh test-file-name)
            (manifold)
            (get-properties)
            (:volume))))
      (finally
        (io/delete-file test-file-name)))))

(deftest test-loft
  (test-props
   {:surface-area 55850.77734375, :volume 54942.234375}
   (loft
    (union
     (difference (circle 10 40)
                 (circle 8 40))
     (difference (circle 20 40)
                 (circle 18 40)))
    (let [n 30]
      (for [i (range (inc n))]
        (-> (frame 1)
            (rotate [0 (- (* i (/ (/ Math/PI 1) n))) 0])
            (translate [50 0 0]))))))

  (test-props
   {:surface-area 3723.955322265625, :volume 16952.8828125}
   (loft [(circle 15 20)
          (square 20 20 true)]
         [(frame 1) (-> (frame 1) (translate [0 0 30]))]))

  (test-props
   {:surface-area 3723.955322265625, :volume 16952.8828125}
   (loft [(circle 15 20)
          (square 20 20 true)]
         [(frame 1) (-> (frame 1) (translate [0 0 30]))]
         :eager-nearest-neighbor) )

  (test-props
   {:surface-area 13741.2900390625, :volume 2407.7626953125}
   (loft
    (reductions
     (fn [m _]
       (assoc m
              :frame (-> (:frame m)
                         (rotate [0 (/ 0.2 2) (/ Math/PI 6)])
                         (translate [0 0 10])
                         (rotate [0 (/ 0.2 2) 0]))))
     {:cross-section (difference (square 8 8 true)
                                 (square 6 6 true))
      :algorithm :eager-nearest-neighbor
      :frame (-> (frame 1)
                 (rotate [(+ (/ Math/PI 10)) 0 0]))}
     (cons (rem (* 1.5 Math/PI) 0.2) (range (quot (* 1.5 Math/PI) 0.2)))))) )
