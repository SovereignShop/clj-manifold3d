(ns clj-manifold3d.core
  (:import
   [manifold3d Manifold]
   [manifold3d.pub DoubleMesh SmoothnessVector Smoothness SimplePolygon]
   [manifold3d.manifold CrossSection MeshIO ExportOptions]
   [manifold3d.glm DoubleVec3 DoubleVec2 DoubleMat4x3 DoubleMat3x2
    DoubleVec3Vector IntegerVec3Vector])
  (:require
   [clj-manifold3d.impl :as impl]))

(defn manifold
  ([] (Manifold.))
  ([^DoubleMesh mesh] (Manifold. mesh)))

(defn smoothness
  ([& {:keys [halfedge smoothness]}]
   (cond-> (Smoothness.)
     halfedge (.halfedge halfedge)
     smoothness (.smoothness smoothness))))

(defn smooth
  ([mesh]
   (smooth mesh []))
  ([mesh sharpened-edges]
   (Manifold/Smooth mesh (let [v (SmoothnessVector.)]
                           (doseq [e sharpened-edges]
                             (.pushBack v e))))))

(defn tetrahedron []
  (Manifold/Tetrahedron))

(defn cube
  ([xyz]
   (cube xyz false))
  ([[x y z] center?]
   (Manifold/Cube (DoubleVec3. x y z) center?))
  ([x y z center?]
   (Manifold/Cube (DoubleVec3. x y z) center?)))

(defn cylinder
  ([height radius]
   (cylinder height radius radius))
  ([height radius-low radius-high]
   (cylinder height radius-low radius-high 0))
  ([height radius-low radius-high circular-segments]
   (cylinder height radius-low radius-high circular-segments false))
  ([height radius-low radius-high circular-segments center?]
   (Manifold/Cylinder height radius-low radius-high circular-segments center?)))

(defn sphere
  ([radius]
   (sphere radius 0))
  ([radius circular-segments]
   (Manifold/Sphere radius circular-segments)))

(defn extrude
  ([cross-section height]
   (extrude cross-section height 0))
  ([cross-section height n-divisions]
   (extrude cross-section height n-divisions 0.0))
  ([cross-section height n-divisions twist-degrees]
   (extrude cross-section height n-divisions twist-degrees (DoubleVec2. 1.0 1.0)))
  ([cross-section height n-divisions twist-degrees scale-top]
   (Manifold/Extrude cross-section height n-divisions twist-degrees scale-top)))

(defn revolve
  ([cross-section]
   (revolve cross-section 0))
  ([cross-section circular-segments]
   (Manifold/Revolve cross-section circular-segments))
  ([cross-section circular-segments degrees]
   (Manifold/Revolve cross-section circular-segments degrees)))

(defn hull
  ([a b]
   (.ConvexHull a b))
  ([a b & more]
   (reduce hull (.ConvexHull a b) more)))

(defn manifold? [x]
  (instance? Manifold x))

(defn cross-section? [x]
  (instance? CrossSection x))

(defn union
  ([a] a)
  ([a b] (impl/union a b))
  ([a b & more]
   (reduce union (union a b) more)))

(defn difference
  ([a] a)
  ([a b] (impl/difference a b))
  ([a b & more]
   (reduce difference (difference a b) more)))

(defn intersection
  ([a] a)
  ([a b] (impl/intersection a b))
  ([a b & more]
   (reduce intersection (intersection a b) more)))

(defn translate [x tv]
  (impl/translate x tv))

(defn rotate [x rv]
  (impl/rotate x rv))

(defn transform [x transform-matrix]
  (impl/transform x transform-matrix))

(defn get-mesh [manifold]
  (.GetMesh manifold))

(defn cross-section
  ([polygon]
   (cross-section polygon :non-zero))
  ([polygon fill-rule]
   (CrossSection. (SimplePolygon/FromArray (double-array (sequence cat polygon)))
                  (case fill-rule
                    :non-zero 0
                    :positive 1
                    :negative 2
                    (throw (IllegalArgumentException. "fill-rule should be :non-zero, :positive or :negative"))))))

(defn circle
  ([r] (CrossSection/Circle r 0))
  ([r circular-segments] (CrossSection/Circle r circular-segments)))

(defn square
  ([x y] (CrossSection/Square (DoubleVec2. x y) false))
  ([x y center?]
   (CrossSection/Square (DoubleVec2. x y) center?)))

(defn offset
  ([section delta]
   (offset section delta :square))
  ([section delta join-type]
   (offset section delta join-type 2.0))
  ([section delta join-type miter-limit]
   (offset section delta join-type miter-limit 0.0))
  ([section delta join-type miter-limit arc-tolerance]
   (.Offset section delta
            (case join-type
              :square 0
              :round 1
              :miter 2)
            miter-limit arc-tolerance)))

(defn export-mesh [mesh filename  & {:keys [faceted]}]
  (MeshIO/ExportMesh filename
                     mesh
                     (cond-> (ExportOptions.)
                       faceted (.faceted true))))

(defn import-mesh
  ([filename]
   (MeshIO/ImportMesh filename false))
  ([filename force-cleanup?]
   (MeshIO/ImportMesh filename force-cleanup?)))
