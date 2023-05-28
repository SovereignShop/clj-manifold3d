(ns clj-manifold3d.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [clj-manifold3d.core :refer [mesh cube get-mesh manifold get-properties mirror union scale
                                         compose
                                         smooth sphere refine cylinder polyhedron export-mesh tetrahedron]]))

(defn- glm-to-vectors [v]
  (for [i (range (.size v))]
    (let [v (.get v i)]
      [(.x v) (.y v) (.z v)])))

(defn about=
  ([x y]
   (about= x y 0.05))
  ([x y eps]
   (< (Math/abs (- x y)) eps)))

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
        c (cylinder 20 r r 200)
        p (get-properties c)]
    ;; Note volume is dependent on nuber of circular segments.
    (is (about= (:volume p) (* Math/PI (Math/pow r 2) 20) 0.2))))

(deftest test-mirror
  (let [c (cube 10 10 10 false)
        m (mirror c [0 1 0])
        verts (.vertPos (get-mesh m))]
    (doseq [i (range (.size verts))]
      (is (not (pos? (.y (.get verts i))))))))

(-> (union (cube 10 10 10 false)
           (mirror (cube 10 10 20 false) [0 1 0]))
    (get-mesh)
    (export-mesh "test.glb"))

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
  (let [man (compose [(tetrahedron) (cube 1 1 1 false) (sphere 1 4)])]
    (-> man (get-mesh) (export-mesh "test.glb"))))
