(ns clj-manifold3d.core
  (:import [manifold3d Manifold]
           [manifold3d.pub DoubleMesh SmoothnessVector Smoothness SimplePolygon]
           [manifold3d.manifold CrossSection MeshIO ExportOptions]
           [manifold3d.glm DoubleVec3 DoubleVec2 DoubleMat4x3 DoubleMat3x2]))

(defn manifold
  ([] (Manifold.))
  ([mesh] (Manifold. mesh)))

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
   (sphere radius))
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
   (Manifold/Revolve cross-section circular-segments)))

(defn manifold? [x]
  (instance? Manifold x))

(defn cross-section? [x]
  (instance? CrossSection x))

(defn- check-csg-args [a b]
  (when-not
      (or (and (manifold? a) (manifold? b))
          (and (cross-section? a) (cross-section? b)))
    (throw (IllegalArgumentException. "A and B must be same type and must be either a Manifold or CrossSection"))))

(defn union
  ([a] a)
  ([a b]
   (check-csg-args a b)
   (.add a b))
  ([a b & more]
   (reduce union (union a b) more)))

(defn difference
  ([a] a)
  ([a b]
   (check-csg-args a b)
   (.subtract a b))
  ([a b & more]
   (reduce difference (difference a b) more)))

(defn intersection
  ([a] a)
  ([a b]
   (check-csg-args a b)
   (.intersect a b))
  ([a b & more]
   (reduce intersection (intersection a b) more)))

(defn translate [x v]
  (cond (manifold? x)
        (.translate x (let [[x y z] v]
                        (DoubleVec3. x y z)))

        (cross-section? x)
        (.translate x (let [[x y] v]
                        (DoubleVec2. x y)))

        :else (throw (IllegalArgumentException. (str "Unknown type: " (type x))))))

(defn rotate [x rv]
  (cond (manifold? x)
        (.rotate x (let [[rx ry rz] rv]
                     (DoubleVec3. rx ry rz)))

        (cross-section? x)
        (.rotate x (let [[rx ry] rv]
                     (DoubleVec2. rx ry)))

        :else (throw (IllegalArgumentException. (str "Unknown type: " (type x))))))

(defn transform [x transform-matrix]
  (cond (manifold? x)
        (.transform ^Manifold x ^DoubleMat4x3 transform-matrix)

        (cross-section? x)
        (.transform ^CrossSection x ^DoubleMat3x2 transform-matrix)

        :else (throw (IllegalArgumentException. (str "Agument must be a Manifold or CrossSection. Recieved: " (type x))))))

(defn get-mesh [manifold]
  (.GetMesh manifold))

(defn export-mesh [filename mesh & {:keys [faceted]}]
  (MeshIO/ExportMesh filename
                     mesh
                     (cond-> (ExportOptions.)
                       faceted (.faceted true))))

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
  ([r] (circle r 10))
  ([r n] r))

(defn square
  ([x y] (square x y true))
  ([x y center?] (m/polygon)))

(defn import-mesh
  ([filename]
   (MeshIO/ImportMesh filename false))
  ([filename force-cleanup?]
   (MeshIO/ImportMesh filename force-cleanup?)))

(comment

  (manifold? (cylinder 100 10))

  (export-mesh
   "test.stl"
   (get-mesh
    (union
     (cube [10 10 10])
     (cube [5 5 20])
     (cube [3 3 30]))))

  (export-mesh
   "test.stl"
   (get-mesh (extrude (difference (cross-section [[-10 -10] [10 -10] [10 10] [-10 10]])
                                  (cross-section [[-5 -5] [5 -5] [5 5] [-5 5]]))
                      100))) 

  )
