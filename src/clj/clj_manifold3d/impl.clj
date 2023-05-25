(ns clj-manifold3d.impl
  (:import [manifold3d Manifold]
           [manifold3d.pub DoubleMesh SmoothnessVector Smoothness SimplePolygon]
           [manifold3d.manifold CrossSection MeshIO ExportOptions]
           [manifold3d.glm DoubleVec3 DoubleVec2 DoubleMat4x3 DoubleMat3x2 MatrixTransforms]))

(defprotocol ITransformable
  (rotate [x rv])
  (translate [x rv])
  (transform [x tf]))

(defprotocol ICSG
  (union [x y])
  (difference [x y])
  (intersection [x y]))

(extend-protocol ICSG
  Manifold
  (union [this o]
    (.add this o))
  (difference [this o]
    (.subtract this o))
  (intersection [this o]
    (.intersect this o))

  CrossSection
  (union [this o]
    (.add this o))
  (difference [this o]
    (.subtract this o))
  (intersection [this o]
    (.intersection this o)))

(extend-protocol ITransformable
  CrossSection
  (rotate [this rv]
    (.Rotate this (DoubleVec2. (nth rv 0) (nth rv 1))))
  (translate [this tv]
    (.Translate this (DoubleVec2. (nth tv 0) (nth tv 1))))
  (transform [this tf]
    (.Transform this tf))

  Manifold
  (rotate [this rv]
    (.Rotate this (nth rv 0) (nth rv 1) (nth rv 2)))
  (translate [this v]
    (.Translate this (DoubleVec3. (nth v 0) (nth v 1) (nth v 2))))
  (transform [this tf]
    (.Transform this tf))

  DoubleMat4x3
  (rotate [this rv]
    (MatrixTransforms/rotate this (DoubleVec3. (nth rv 0) (nth rv 1) (nth rv 2))))
  (translate [this tv]
    (MatrixTransforms/translate this (DoubleVec3. (nth tv 0) (nth tv 1) (nth tv 2))))
  (transform [this matrix]
    (MatrixTransforms/transform this matrix)))
