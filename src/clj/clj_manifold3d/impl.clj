(ns clj-manifold3d.impl
  (:import [manifold3d Manifold]
           [manifold3d.pub DoubleMesh SmoothnessVector Smoothness SimplePolygon]
           [manifold3d.manifold CrossSection MeshIO ExportOptions]
           [manifold3d.glm DoubleVec3 DoubleVec2 DoubleMat4x3 DoubleMat3x2]))

(defprotocol IFrame
  (rotate [x rv])
  (translate [x rv])
  (transform [x tf]))

(defprotocol ICSG
  (union [x y])
  (difference [x y])
  (intersection [x y]))

(extend-protocol IFrame
  CrossSection
  (rotate [this rv] this)
  (translate [this rv] this)
  (transform [this tf] this))

(extend-protocol ICSG
  Manifold
  (union [this o])
  (difference [this o])
  (intersection [this o])

  CrossSection
  (union [this o])
  (difference [this o])
  (intersection [this o]))

