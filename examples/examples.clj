(ns examples
  (:require
   [clj-manifold3d.core :as m]))

(require '[clj-manifold3d.core :as m])

(def mesh-material (m/material :roughness 0.0 :metalness 0.0 :color [0.0 0.7 0.7 1]))

;; Manifolds

(let [cube (m/cube 20 20 20 true)
      sphere (m/sphere 12 30)]
  (-> (m/union
       (-> (m/union cube sphere)
           (m/translate [30 0 0]))
       (-> (m/difference cube sphere)
           (m/translate [-30 0 0]))
       (m/intersection cube sphere))
      (m/get-mesh)
      (m/export-mesh "manifolds.glb" :material mesh-material)))

;; Cross Sections


(-> (m/cross-section (cons [0 0]
                           (for [i (range 18)]
                             [(* 20 (Math/cos (* i (/ (* 2 Math/PI) 19))))
                              (* 20 (Math/sin (* i (/ (* 2 Math/PI) 19))))])))
    (m/extrude 1)
    (m/get-mesh)
    (m/export-mesh "cross-section.glb" :material mesh-material))

(-> (m/cross-section [(for [i (range 20)]
                        [(* 20 (Math/cos (* i (/ (* 2 Math/PI) 19))))
                         (* 20 (Math/sin (* i (/ (* 2 Math/PI) 19))))])
                      (reverse
                       (for [i (range 20)]
                         [(* 18 (Math/cos (* i (/ (* 2 Math/PI) 19))))
                          (* 18 (Math/sin (* i (/ (* 2 Math/PI) 19))))]))])
    (m/extrude 1)
    (m/get-mesh)
    (m/export-mesh "cross-section-with-hole.glb" :material mesh-material))


;; 2D Hull

(-> (m/hull
     (m/circle 5)
     (-> (m/square 10 10 true)
         (m/translate [30 0])))
    (m/extrude 10)
    (m/get-mesh)
    (m/export-mesh "hull2D.glb" :material mesh-material))

;; 3D Hull

(-> (m/hull (m/cylinder 2 12 12 120)
            (-> (m/sphere 4 120)
                (m/translate [0 0 20])))
    (m/get-mesh)
    (m/export-mesh "hull3D.glb" :material mesh-material))

;; Revolution

(let [m (-> (m/cross-section [[-10 0] [10 0] [0 10]])
            (m/translate [30 0]))]
  (-> (m/difference m (m/offset m -1))
      (m/revolve 50 135)
      (m/get-mesh)
      (m/export-mesh "revolve.glb" :material mesh-material)))


;; Polyhedron

(-> (m/polyhedron [[0 0 0]
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
    (m/get-mesh)
    (m/export-mesh "polyhedron-cube.glb" :material mesh-material))

;; Transform

(-> (m/cylinder 50 5)
    (m/transform (-> (m/frame 1)
                     (m/rotate [0 (/ Math/PI 4) 0])
                     (m/translate [0 0 30])))
    (m/get-mesh)
    (m/export-mesh "transform.glb" :material mesh-material))

;; Loft

(-> (let [c (m/difference (m/square 10 10 true) (m/square 8 8 true))]
      (m/loft [c (m/scale c [1.5 1.5]) c]
              [(m/frame 1)
               (m/translate (m/frame) [0 0 15])
               (m/translate (m/frame) [0 0 30])]))
    (m/get-mesh)
    (m/export-mesh "loft.glb" :material mesh-material))

(-> (m/loft [(m/circle 20 15)
             (m/square 30 30 true)
             (m/circle 20 20)]
            [(m/frame 1)
             (m/translate (m/frame) [0 0 15])
             (m/translate (m/frame) [0 0 30])])
    (m/get-mesh)
    (m/export-mesh "monomorphic-loft.glb" :material mesh-material))


(-> (m/loft [{:cross-section (m/circle 50 12)
              :frame (m/frame)}
             {:frame (m/translate (m/frame) [0 0 20])}
             {:cross-section (m/circle 46 12)}
             {:frame (m/translate (m/frame) [0 0 3])}])
    (m/get-mesh)
    (m/export-mesh "single-arity-loft.glb" :material mesh-material))

;; Text

(-> (m/text "resources/fonts/Cinzel-Regular.ttf" "Manifold" 10 20 :non-zero)
    (m/scale-to-height 100)
    (m/extrude 20)
    (m/get-mesh)
    (m/export-mesh "text.glb" :material mesh-material))

;; Slice

(-> (m/slice (m/scale (m/tetrahedron) [5 10 15]))
    (m/extrude 1/2)
    (m/get-mesh)
    (m/export-mesh "slice.glb" :material mesh-material))

(-> (m/union
     (for [[i slice] (map-indexed vector (m/slices (m/scale (m/tetrahedron) [5 10 15]) 5 10 10) )]
       (-> slice
           (m/extrude 1/8)
           (m/translate [0 0 (* i 0.5)]))))
    (m/get-mesh)
    (m/export-mesh "slices.glb" :material mesh-material))


;; Surface

(defn sinewave-heatmap
  "Generates a 3D sinewave heatmap.
  The output is a vector of vectors representing a square matrix.
  Each cell represents the height at that x/y coordinate based on a sinewave.

  Args:
  - size: The size of the matrix (width and height).
  - frequency: Frequency of the sinewave (controls the number of wave oscillations).
  - amplitude: Amplitude of the sinewave (controls the height of the wave).
  - phase: Phase shift of the sinewave.

  Returns a matrix where each value is the sinewave height at that coordinate."
  [size frequency amplitude phase]
  (vec
   (for [y (range size)]
     (vec
      (for [x (range size)]
        (+ (+ 10 amplitude)
           (* amplitude
              (Math/sin
               (+ (* frequency (/ x size))
                  (* frequency (/ y size))
                  phase)))))))))

(-> (sinewave-heatmap 50 10 5 0)
    (m/surface 1.0)
    (m/get-mesh)
    (m/export-mesh "sine-wave-surface.glb" :material mesh-material))

;; Color

(->
 (m/difference
  (m/color (m/cube 20 20 20) [0 0 1 1])
  (m/color (m/cube 20 20 40 true) [1 0 0 1]))
 (m/get-mesh)
 (m/export-mesh "colored-manifold.glb"
                :material (m/material :roughness 0.0 :metalness 0.0 :color-idx 0)))

;; Compose

(-> (m/compose
     (m/color (m/cube 30 30 30 true)
              [1 1 1 0.5])
     (m/color (m/sphere 12 40)
              [0 0 1 1.0]))
    (m/get-mesh)
    (m/export-mesh "compose.glb"
                   :material (m/material :roughness 0.0 :metalness 0.0 :color-idx 0 :alpha-idx 3)))

;; Three Point Arcs and Circles

(let [p1 [0 0] p2 [1 12] p3 [15 0]]
  (-> (m/difference
       (m/circle p1 p2 p3 100)
       (m/union
        (for [p [p1 p2 p3]]
          (-> (m/circle 1 10)
              (m/translate p)))))
      (m/extrude 1)
      (m/get-mesh)
      (m/export-mesh "three-point-circle.glb" :material mesh-material)))

(let [p1 [0 0] p2 [2 5] p3 [-6 10]]
  (-> (m/union
       (m/three-point-arc p1 p2 p3 20)
       (m/union
        (for [p [p1 p2 p3]]
          (-> (m/circle 1 10)
              (m/translate p)))))
      (m/extrude 1)
      (m/get-mesh)
      (m/export-mesh "three-point-arc.glb" :material mesh-material)))


;; Get Vertices

(-> (let [verts (m/get-vertices (m/cube 20 20 20 true))]
      (m/union
       (for [vert verts]
         (-> (m/sphere 2 20)
             (m/translate vert)))))
    (m/get-mesh)
    (m/export-mesh "get-vertices.glb" :material mesh-material))

;; Get Halfedges

(-> (let [m (m/cube 40 40 40 true)
          verts (m/get-vertices m)
          halfedges (m/get-halfedges m)]
      (m/compose
       (cons (m/color m [0 1 0 0.4])
             (for [halfedge halfedges]
               (let [v1 (nth verts (:start-vert halfedge))
                     v2 (nth verts (:end-vert halfedge))
                     m (m/hull
                        (m/translate (m/sphere 1 15) v1)
                        (m/translate (m/sphere 3 15) v2))]
                 (if (m/is-forward halfedge)
                   (m/color m [1 0 0 0.6])
                   (m/color m [0 0 1 0.6])))))))
    (m/get-mesh)
    (m/export-mesh "get-halfedges.glb" :material (m/material :roughness 0.0
                                                             :metalness 0.0
                                                             :color-idx 0
                                                             :alpha-idx 3)))

(defn next-halfedge-idx [curr-halfedge-idx]
  (+ (* 3 (quot curr-halfedge-idx 3))
     (mod (inc curr-halfedge-idx) 3)))
