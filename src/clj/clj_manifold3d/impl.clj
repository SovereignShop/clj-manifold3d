(ns clj-manifold3d.impl
  (:import [manifold3d Manifold ManifoldVector]
           [manifold3d.pub SmoothnessVector Smoothness SimplePolygon OpType SimplePolygon Polygons]
           [manifold3d.manifold CrossSection MeshIO ExportOptions CrossSectionVector CrossSection$FillRule]
           [manifold3d.linalg DoubleVec3 DoubleVec2 DoubleMat3x4 DoubleMat2x3 MatrixTransforms]))

(defprotocol ITransformable
  (rotate [x rv])
  (translate [x rv])
  (transform [x tf]))

(defprotocol ICSG
  (batch-boolean [x xs op])
  (union [x y])
  (difference [x y])
  (intersection [x y]))

(defprotocol ICSGConvertable
  (to-csg [this] "Converts to a CSG object (Manifold or CrossSection)"))

(defprotocol IToPolygons
  (to-polygons [this]))

(extend-protocol ICSGConvertable
  Manifold
  (to-csg [this] this)
  CrossSection
  (to-csg [this] this)
  Polygons
  (to-csg [this] (CrossSection. this (.ordinal CrossSection$FillRule/NonZero)))
  SimplePolygon
  (to-csg [this] (CrossSection. (doto (Polygons.) (.pushBack this))
                                (.ordinal CrossSection$FillRule/NonZero))))

(extend-protocol ICSG
  Manifold
  (batch-boolean [this xs op]
    (Manifold/BatchBoolean
     (let [v (ManifoldVector.)]
       (doseq [x (cons this xs)]
         (.pushBack v x))
       v)
     op))
  (union [this o]
    (.add this o))
  (difference [this o]
    (.subtract this o))
  (intersection [this o]
    (.intersect this o))

  CrossSection
  (to-csg [this] this)
  (batch-boolean [this xs op]
    (CrossSection/BatchBoolean
     (let [v (CrossSectionVector.)]
       (doseq [x (cons this xs)]
         (.pushBack v x))
       v)
     op))
  (union [this o]
    (.add this o))
  (difference [this o]
    (.subtract this o))
  (intersection [this o]
    (.intersect this o)))

(extend-protocol IToPolygons
  CrossSection
  (to-polygons [this]
    (.toPolygons this))
  SimplePolygon
  (to-polygons [this] ^Polygons (doto (Polygons.) (.pushBack this)))
  Polygons
  (to-polygons [this] this))

(extend-protocol ITransformable
  CrossSection
  (rotate [this degrees]
    (.rotate this degrees))
  (translate [this tv]
    (.translate this (DoubleVec2. (nth tv 0) (nth tv 1))))
  (transform [this tf]
    (.transform this tf))

  Manifold
  (rotate [this rv]
    (.rotate this (nth rv 0) (nth rv 1) (nth rv 2)))
  (translate [this v]
    (.translate this (DoubleVec3. (nth v 0) (nth v 1) (nth v 2))))
  (transform [this tf]
    (.transform this tf))

  DoubleMat3x4
  (rotate [this rv]
    (MatrixTransforms/Rotate this (DoubleVec3. (nth rv 0) (nth rv 1) (nth rv 2))))
  (translate [this tv]
    (MatrixTransforms/Translate this (DoubleVec3. (nth tv 0) (nth tv 1) (nth tv 2))))
  (transform [this matrix]
    (MatrixTransforms/CombineTransforms this matrix))

  DoubleMat2x3
  (rotate [this angle]
    (MatrixTransforms/Rotate this angle))
  (translate [this tv]
    (MatrixTransforms/Translate this (DoubleVec2. (nth tv 0) (nth tv 1))))
  (transform [this matrix]
    (MatrixTransforms/CombineTransforms this matrix)))
