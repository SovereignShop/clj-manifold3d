(ns clj-manifold3d.core
  "This library defines a wrapper over Manifold for clojure and clojurescript.
  Refer to the original library for complete documentation.

  This library aspires to achieve code capatibility between Clojure and ClojureScript so that models
  build in the more friendly Java environment can be shared and played with or parameterized in the javascript
  environment. However, there are challenges in the way the Manifold js library is provided
  as a promise. To (mostly) support this, this library elects to accept promises at the API level.
  Working with promises can be pretty annoying, especially without a type system that supports
  them well. For this reason, the CLJS API generally also works on non-promise objects."
  #?(:clj
     (:import
      [manifold3d Manifold MeshUtils MeshUtils$LoftAlgorithm ManifoldVector FloatVector UIntVector]
      [manifold3d.pub  SmoothnessVector Smoothness SimplePolygon Polygons PolygonsVector OpType]
      [manifold3d.manifold CrossSection CrossSectionVector Material ExportOptions MeshIO MeshGL]
      [manifold3d.linalg DoubleVec3 DoubleVec2 DoubleMat3x4 DoubleMat2x3 DoubleMat3x4Vector
       MatrixTransforms DoubleVec4]
      [java.nio ByteBuffer ByteOrder DoubleBuffer IntBuffer]))
  #?(:clj
     (:require
      [clj-manifold3d.impl :as impl]
      [clj-manifold3d.utils :as u])
     :cljs
     (:require
      [promesa.core :as p]
      ["/clj_manifold3d/manifold" :as manifold3d])))

#?(:cljs
   (def ^:dynamic *manifold-module* (manifold3d)))

#?(:cljs
   (defn update-manifold [manifold f]
     (if (promise? manifold)
       (.then manifold f)
       (f manifold))))

#?(:clj
   (defn- double-vec2-sequence-to-native-double-buffer
     "Maps a sequence of 3-sequences to a flat row-major double buffer."
     [col]
     (let [buf (-> (ByteBuffer/allocateDirect (* (count col) 2 Double/BYTES))
                   (.order (ByteOrder/nativeOrder))
                   (.asDoubleBuffer))]
       (doseq [[^double a ^double b] col]
         (.put buf a)
         (.put buf b))
       (.flip buf))))

#?(:clj
   (defn- double-vec3-sequence-to-native-double-buffer
     "Maps a sequence of 3-sequences to a flat row-major double buffer."
     [col]
     (let [buf (-> (ByteBuffer/allocateDirect (* (count col) 3 Double/BYTES))
                   (.order (ByteOrder/nativeOrder))
                   (.asDoubleBuffer))]
       (doseq [[^double a ^double b ^double c] col]
         (.put buf a)
         (.put buf b)
         (.put buf c))
       (.flip buf))))

#?(:clj
   (defn- vec3-sequence-to-native-float-buffer
     "Maps a sequence of 3-sequences to a flat row-major double buffer."
     [col]
     (let [buf (-> (ByteBuffer/allocateDirect (* (count col) 3 Float/BYTES))
                   (.order (ByteOrder/nativeOrder))
                   (.asFloatBuffer))]
       (doseq [[^float a ^float b ^float c] col]
         (.put buf a)
         (.put buf b)
         (.put buf c))
       (.flip buf))))



#?(:clj
   (defn- square-sequences-to-native-float-buffer
     "Maps a sequence of 3-sequences to a flat row-major double buffer."
     [col]
     (let [buf (-> (ByteBuffer/allocateDirect (* (count col) (count (first col)) Float/BYTES))
                   (.order (ByteOrder/nativeOrder))
                   (.asFloatBuffer))]
       (doseq [row col
               ^double a row]
         (.put buf a))
       (.flip buf))))

#?(:clj
   (defn- double-vec4-sequence-to-native-double-buffer
     "Maps a sequence of 4-sequences to a flat native-ordered row-major double buffer."
     [col]
     (let [buf (-> (ByteBuffer/allocateDirect (* (count col) 4 Double/BYTES))
                   (.order (ByteOrder/nativeOrder))
                   (.asDoubleBuffer))]
       (doseq [[^double a ^double b ^double c ^double d] col]
         (.put buf a)
         (.put buf b)
         (.put buf c)
         (.put buf d))
       (.flip buf))))

#?(:clj
   (defn- integer-vec3-sequence-to-native-integer-buffer
     "Maps a sequence of 3-sequences to a flat native-ordered row-major integer buffer."
     [col]
     (let [buf (-> (ByteBuffer/allocateDirect (* (count col) 3 Integer/BYTES))
                   (.order (ByteOrder/nativeOrder))
                   (.asIntBuffer))]
       (doseq [[^int a ^int b ^int c] col]
         (.put buf a)
         (.put buf b)
         (.put buf c))
       (.flip buf))))

#?(:clj
   (defn- vec3-sequence-to-native-long-buffer
     "Maps a sequence of 3-sequences to a flat native-ordered row-major integer buffer."
     [col]
     (let [buf (-> (ByteBuffer/allocateDirect (* (count col) 3 Long/BYTES))
                   (.order (ByteOrder/nativeOrder))
                   (.asLongBuffer))]
       (doseq [[^long a ^long b ^long c] col]
         (.put buf a)
         (.put buf b)
         (.put buf c))
       (.flip buf))))


(defn manifold?
  "Returns true if `x` is a `Manifold`."
  [x]
  #?(:clj (instance? Manifold x)
     :cljs (p/let [mod *manifold-module*
                   i x]
             (instance? mod.Manifold i))))

#?(:clj
   (defn csg?
     "Retruns true if `x` is convertable to csg object."
     [x]
     (satisfies? impl/ICSGConvertable x)))

(defn cross-section?
  "Returns true if `x` is a `CrossSection`."
  [x]
  #?(:clj (instance? CrossSection x)
     :cljs (p/let [mod *manifold-module*
                   i x]
             (instance? mod.CrossSection i))))

#?(:clj
   (defn mesh
     "Convenience function to create a mesh from sequences. `:vert-pos` is a
  vector of [x y z] vertices. `:tri-verts` is a vector of [idx1 idx2 idx3]
  vertex indices representing a triangular face. For maximum java performance
  use FromBuffer constructors directly and use natively ordered structures that
  can provide java.nio buffers. If necessary, write meshing alorthims in c++
  using GLM directly and bind to them."
     [& {:keys [tri-verts vert-pos]}]
     (cond-> (MeshGL.)
       ;; true (doto (.numProp 3))
       tri-verts (doto (.triVerts (UIntVector/FromBuffer (vec3-sequence-to-native-long-buffer tri-verts))))
       vert-pos (doto (.vertProperties (FloatVector/FromBuffer (vec3-sequence-to-native-float-buffer vert-pos)))))))

(defn manifold
  "Creates a `Manifold` ."
  ([] #?(:clj (Manifold.)))
  ([mesh]
   #?(:clj (Manifold. ^MeshGL mesh)
      :cljs (update-manifold *manifold-module*
                             (fn [mod]
                               (.setup mod)
                               (mod.Manifold. (mod.Mesh. (clj->js mesh))))))))
#?(:clj
   (defn is-empty? [x]
     (cond (manifold? x) (.isEmpty ^Manifold x)
           (cross-section? x) (.isEmpty ^CrossSection x)
           :else (throw (IllegalArgumentException. (str "Should be Manifold or CrossSection. Recieved: " (type x)))))))

#?(:clj
   (defn tetrahedron
     "Creates a tetrahedron with side length of 1."
     []
     (Manifold/Tetrahedron)))

#?(:clj
   (defn polyhedron
     "Creates a manifold from a polyhedron. `verts` is a sequence of [x y z]
  vertices. `faces` is a sequence of sequences of indices into `verts`
  corresponding to a face. Faces can include more then 3 vertices. Face vertices
  must be coplanar and ordered CCW so the normal points outward by the right-hand rule.

  Note it is significantly faster to construct a mesh directly with triangular
  faces."
     [verts faces]
     (let [^DoubleBuffer vert-buf (double-vec3-sequence-to-native-double-buffer verts)
           face-counts (map count faces)
           n-face-verts (apply + face-counts)
           ^IntBuffer
           face-buf (let [buf (-> (ByteBuffer/allocateDirect (* n-face-verts Integer/BYTES))
                                  (.order (ByteOrder/nativeOrder))
                                  (.asIntBuffer))]
                      (doseq [c faces
                              ^int x c]
                        (.put buf x))
                      (.flip buf))
           ^IntBuffer
           face-lengths (let [buf (-> (ByteBuffer/allocateDirect (* (count faces) Integer/BYTES))
                                      (.order (ByteOrder/nativeOrder))
                                      (.asIntBuffer))]
                          (doseq [^int c face-counts]
                            (.put buf c))
                          (.flip buf))]
       (MeshUtils/PolyhedronFromBuffers vert-buf (count verts) face-buf face-lengths (count faces)))))

#?(:clj
   (defn surface
     "Creates a Manifold from a heat map defined as a square matrix with each cell representing height.
   `heatmap` is a sequence of sequences of equal length. `pixel-width` specifies the width each pixel
    cooresponds to in the resulting manifold."
     ([heatmap]
      (surface heatmap 1.0))
     ([heatmap pixel-width]
      (let [width (count heatmap)
            height (count (first heatmap))
            heat-values (square-sequences-to-native-float-buffer heatmap)]
        (MeshUtils/CreateSurface heat-values 1 width height pixel-width)))))

#?(:clj
   (defn ply-file-to-surface
     "Load a ply file and convert it to a rectangular heat map."

     ([filepath]
      (MeshUtils/PlyToSurface filepath 10.0 20.0 304.8))
     ([filepath cell-size]
      (MeshUtils/PlyToSurface filepath cell-size 20.0 304.8))
     ([filepath cell-size z-offset]
      (MeshUtils/PlyToSurface filepath cell-size z-offset 304.8))
     ([filepath cell-size z-offset scale-factor]
      (MeshUtils/PlyToSurface filepath cell-size z-offset scale-factor))))

#?(:clj
   (defn load-surface
     ([filename]
      (MeshUtils/CreateSurface filename))
     ([filename pixel-width]
      (MeshUtils/CreateSurface filename pixel-width))))

#?(:clj
   (defn load-image
     ([filename depth]
      (MeshUtils/LoadImage filename depth))
     ([filename depth pixel-width]
      (MeshUtils/LoadImage filename depth pixel-width))))

(defn cube
  "Creates a cube with specified dimensions.

  Overloaded Function Definitions:

  1. ([xyz]): Takes a 3-element vector [x y z] representing the dimensions of the cube, with the origin as the center.
  2. ([xyz center?]): Takes a 3-element vector [x y z] and a boolean value `center?` that if true, sets the origin as the center of the cube.
  3. ([x y z center?]): Takes the dimensions `x`, `y`, `z` and a boolean value `center?` that if true, sets the origin as the center of the cube."
  ([[x y z]]
   (cube x y z false))
  ([[x y z] center?]
   (cube x y z center?))
  ([x y z]
   (cube x y z false))
  ([x y z center?]
   #? (:clj (Manifold/Cube (DoubleVec3. x y z) center?)
       :cljs (update-manifold *manifold-module*
                              (fn [module]
                                (.setup module)
                                (.cube module #js [x y z] center?))))))

(defn cylinder
    "Creates a cylinder with specified dimensions.

  Overloaded Function Definitions:

  1. ([height radius]): Creates a cylinder with given `height` and `radius`.
  2. ([height radius-low radius-high]): Creates a conical cylinder with `height`, `radius-low` at the bottom, and `radius-high` at the top.
  3. ([height radius-low radius-high circular-segments]): Same as above, but `circular-segments` defines the number of divisions around the cylinder.
  4. ([height radius-low radius-high circular-segments center?]): Same as above, but if `center?` is true, the origin is set as the center of the cylinder."
  ([height radius]
   (cylinder height radius radius))
  ([height radius-low radius-high]
   (cylinder height radius-low radius-high 0))
  ([height radius-low radius-high circular-segments]
   (cylinder height radius-low radius-high circular-segments false))
  ([height radius-low radius-high circular-segments center?]
   #?(:clj (Manifold/Cylinder height radius-low radius-high circular-segments center?)
      :cljs (update-manifold  *manifold-module*
                              (fn [module]
                                (.setup module)
                                (.cylinder module height radius-low radius-high circular-segments center?))))))

(defn sphere
  "Creates a sphere with given `radius`. `circular-radius` determins the number of segments around z-axis and y-axis."
  ([radius]
   (sphere radius 0))
  ([radius circular-segments]
   #?(:clj (Manifold/Sphere radius circular-segments)
      :cljs (update-manifold *manifold-module*
                             (fn [module]
                               (.setup module)
                               (.sphere module radius circular-segments))))))

(defn extrude
  "Creates a `Manifold` by extruding a `CrossSection` to the specified `height`.

  `n-divisions` specifies the number of vertical divisions. It is mostly only relevant when also applying `twist-degrees` or `scale-top`.
  `twist-degrees` specifies how many degrees to twist around z-axis while extruding
  `scale-top` A pair [x y] that scales the cross section at the top of the extrusion.
  "
  ([cross-section height]
   (extrude cross-section height 0))
  ([cross-section height n-divisions]
   (extrude cross-section height n-divisions 0.0))
  ([cross-section height n-divisions twist-degrees]
   (extrude cross-section height n-divisions twist-degrees [1.0 1.0]))
  ([cross-section height n-divisions twist-degrees scale-top]
   #?(:clj
      (Manifold/Extrude (impl/to-polygons cross-section)
                        height
                        n-divisions
                        twist-degrees
                        (DoubleVec2. (nth scale-top 0) (nth scale-top 1)))
      :cljs (update-manifold (js/Promise.all [*manifold-module* cross-section])
                             (fn [[module cross-section]]
                               (.setup module)
                               (.extrude module (clj->js cross-section) height n-divisions twist-degrees (clj->js scale-top)))))))

(defn smooth
  "Constructs a smooth version of the input mesh by creating tangents. See manifold documentation
  for more details."
  ([mesh]
   (smooth mesh []))
  ([mesh sharpened-edges]
   #?(:clj (Manifold/Smooth ^MeshGL mesh
                            (let [v (SmoothnessVector.)]
                              (doseq [{:keys [smoothness halfedge]} sharpened-edges]
                                (.pushBack v (doto (Smoothness.)
                                               (.halfedge halfedge)
                                               (.smoothness smoothness))))
                              v))
      :cljs (update-manifold *manifold-module*
                             (fn [module]
                               (.setup module)
                               (.smooth module mesh (clj->js sharpened-edges)))))))

#?(:clj
   (defn smooth-out
     "Smooths out the Manifold by filling in the halfedgeTangent vectors. The
  geometry will remain unchanged until refine or refine-to-length is called to
interpolate the surface. This version uses the geometry of the triangles and
pseudo-normals to define the tangent vectors.

  `min-sharp-angle` degrees, default 60. Any edges with angles greater than
      this value will remain sharp. The rest will be smoothed to G1 continuity,
      with the caveat that flat faces of three or more triangles will always remain
      flat. With a value of zero, the model is faceted, but in this case there is
      no point in smoothing.

  `min-smoothness` range: 0 - 1, default 0. The smoothness applied to sharp
      angles. The default gives a hard edge, while values > 0 will give a small
      fillet on these sharp edges. A value of 1 is equivalent to a minSharpAngle of
      180 - all edges will be smooth."
     ([^Manifold manifold]
      (smooth-out manifold 60))
     ([^Manifold manifold min-sharp-angle]
      (smooth-out manifold min-sharp-angle 0))
     ([^Manifold manifold min-sharp-angle min-smoothness]
      (.smoothOut manifold min-sharp-angle min-smoothness))))

(defn mirror
  "Mirrors manifold/cross-section over the plane/line desribed by the normal."
  ([obj normal]
   #?(:clj (let [csg (impl/to-csg obj)]
             (cond (manifold? csg) (.mirror ^Manifold csg (DoubleVec3. (nth normal 0) (nth normal 1) (nth normal 2)))
                   (cross-section? csg) (.mirror ^CrossSection csg (DoubleVec2. (nth normal 0) (nth normal 1)))
                   :else
                   (throw (IllegalArgumentException. (str "Must be Manifold or CrossSection. Recieved: " (type csg))))))
      :cljs (update-manifold obj
                             (fn [man]
                               (.mirror man (clj->js normal)))))))

#?(:clj
   (defn refine
     "partitions every edge of the Manifold into `n` edges of equal length. CLJ only."
     ([manifold n]
      (.refine ^Manifold (impl/to-csg manifold) n))))

#?(:clj
   (defn calculate-normals
     "Fills in vertex properties for normal vectors, calculated from the mesh
 geometry. Flat faces composed of three or more triangles will remain flat.

  `normal-idx` The property channel in which to store the X
 values of the normals. The X, Y, and Z channels will be sequential. The
 property set will be automatically expanded such that NumProp will be at
 least normalIdx + 3.

  `min-sharp-angle` Any edges with angles greater than this value will
 remain sharp, getting different normal vector properties on each side of the
 edge. By default, no edges are sharp and all normals are shared. With a value
 of zero, the model is faceted and all normals match their triangle normals,
 but in this case it would be better not to calculate normals at all."
     ([^Manifold manifold normal-idx min-sharp-angle]
      (.calculateNormals manifold normal-idx min-sharp-angle))))

#?(:clj
   (defn refine-to-length
     "Increase the density of the mesh by splitting each edge into pieces of
roughly the input `length`. Interior verts are added to keep the rest of the
triangulation edges also of roughly the same length. If halfedgeTangents are
present (e.g. from the Smooth() constructor), the new vertices will be moved
to the interpolated surface according to their barycentric coordinates."
     [^Manifold manifold length]
     (.refineToLength manifold length)))

(defn revolve
  "Revolve a `CrossSection` around the y-axis to create a `Manifold`.

  Overloaded Function Definitions:

  1. ([cross-section]): Revolves the `cross-section` 360 degrees around the y-axis.
  2. ([cross-section circular-segments]): Revolves the `cross-section` and divides the revolution into `circular-segments` segments.
  3. ([cross-section circular-segments degrees]): Revolves the `cross-section` around the y-axis for `degrees` degrees and divides the revolution into `circular-segments` segments."
  ([cross-section]
   (revolve cross-section 0))
  ([cross-section circular-segments]
   (revolve cross-section circular-segments 360.0))
  ([cross-section circular-segments degrees]
   #?(:clj (Manifold/Revolve (impl/to-polygons cross-section) circular-segments degrees)
      :cljs (.then (js/Promise.all #js [*manifold-module* cross-section])
                   (fn [[module cross-section]]
                     (.revolve module cross-section circular-segments degrees))))))

#?(:cljs
   (defn warp
     "Returns a new `Manifold` with `warp` function applied to each vertex. CLJS only."
     [manifold func]
     (update-manifold manifold (fn [man] (.warp man func)))))

(defn hull
  "Takes one or more Manifolds or CrossSections and returns their convex hull."
  #?(:clj ([a]
           (if (sequential? a) (apply hull a)
               (let [csg (impl/to-csg a)]
                 (cond (manifold? csg) (.convexHull ^Manifold csg)
                       (cross-section? csg) (.convexHull ^CrossSection csg)
                       :else
                       (throw (IllegalArgumentException. (str "Must be Manifold or CrossSection. Recieved: " (type csg)))))))))
  ([a b]
   #?(:clj (let [ca (impl/to-csg a)
                 cb (impl/to-csg b)]
             (cond (manifold? ca)
                   (let [v (doto (ManifoldVector.)
                             (.pushBack ca)
                             (.pushBack cb))]
                     ^Manifold (Manifold/ConvexHull ^ManifoldVector v))

                   (cross-section? ca)
                   (let [v (doto (CrossSectionVector.)
                             (.pushBack ca)
                             (.pushBack cb))]
                     ^CrossSection  (CrossSection/ConvexHull ^CrossSectionVector v))

                   :else (throw (IllegalArgumentException. (str "Must be Manifold or CrossSection. Recieved: " (type ca))))))
      :cljs (.then (js/Promise.all #js [a b])
                   (fn [[a b]]
                     (.convexHull a b)))))
  ([a b & more]
   #?(:clj
      (if (manifold? a)
        (let [^ManifoldVector v (ManifoldVector.)]
          (doseq [man (list* a b more)]
            (.pushBack v man))
          ^Manifold (Manifold/ConvexHull v))
        (let [^CrossSectionVector v (CrossSectionVector.)]
          (doseq [section (list* a b more)]
            (.pushBack v section))
          ^CrossSection (CrossSection/ConvexHull v)))
      :cljs (reduce hull (hull a b) more))))

(defn union
  "Returns the union of input Manifolds or CrossSections."
  ([a] (if (sequential? a) (apply union a) a))
  ([a b]
   #?(:clj (impl/union (impl/to-csg a) #?(:clj (impl/to-csg b) :cljs b))
      :cljs (.then (js/Promise.all #js [*manifold-module* a b])
                   (fn [[module & args]]
                     (.setup module)
                     (.union module args)))))
  ([a b & more]
   #?(:clj (impl/batch-boolean (impl/to-csg a)
                               (cons (impl/to-csg b) (map impl/to-csg more))
                               OpType/Add)
      :cljs (.then (js/Promise.all (list* *manifold-module* a b more))
                   (fn [[module & args]]
                     (.setup module)
                     (.union module args))))))

(defn difference
  "Returns the difference between first input `Manifold` or `CrossSection` and rest."
  ([a] (if (sequential? a) (apply difference a) #?(:clj (impl/to-csg a) :cljs a)))
  ([a b]
   #?(:clj (impl/difference (impl/to-csg a) (impl/to-csg b))
      :cljs (.then (js/Promise.all #js [*manifold-module* a b])
                   (fn [[module & args]]
                     (.setup module)
                     (.difference module args)))))
  ([a b & more]
   #?(:clj (impl/batch-boolean (impl/to-csg a)
                               (cons (impl/to-csg b) (map impl/to-csg more))
                               OpType/Subtract)
      :cljs (.then (js/Promise.all (list* *manifold-module* a b more))
                   (fn [[module & args]]
                     (.setup module)
                     (.difference module args))))))

(defn intersection
  "Returns the intersection of all input `Manifold`s or `CrossSection`s."
  ([a] (if (sequential? a) (apply intersection a) #?(:clj (impl/to-csg a) :cljs a)))
  ([a b] #?(:clj (impl/intersection (impl/to-csg a) (impl/to-csg b))
            :cljs (.then (js/Promise.all #js [*manifold-module* a b])
                         (fn [[module & args]] (.intersection module args)))))
  ([a b & more]
   #?(:clj (impl/batch-boolean (impl/to-csg a)
                               (cons (impl/to-csg b) (map impl/to-csg more))
                               OpType/Intersect)
      :cljs (.then (js/Promise.all (list* *manifold-module* a b more))
                   (fn [[module & args]]
                     (.setup module)
                     (.intersection module args))))))

(defn compose
  "Purely topological join of a sequence of manifolds. Care should be taken to avoid overlapping results."
  [manifolds]
  #?(:clj (if (manifold? (first manifolds))
            (Manifold/Compose (let [v (ManifoldVector.)]
                                (doseq [man manifolds]
                                  (.pushBack v man))
                                v))
            (CrossSection/Compose (let [v (CrossSectionVector.)]
                                    (doseq [man manifolds]
                                      (.pushBack v man))
                                    v)))
     :cljs (update-manifold *manifold-module*
                            (fn [module]
                              (.setup module)
                              (.compose (clj->js manifolds))))))

(defn decompose
  "Inverse of compose."
  [obj]
  #?(:clj (cond (manifold? obj) (seq (.decompose ^Manifold obj))
                (cross-section? obj) (seq (.decompose ^CrossSection obj))
                :else (throw (IllegalArgumentException. (str "Input must be Manifold or CrossSection. Recieved: " (type obj)))))
     :cljs (update-manifold obj
                            (fn [x] (.decompose x)))))

(defn scale
  "Scale `obj` (Manifold or CrossSection). `sv` is a [scale-x scale-y] vector for cross-sections and a
  [scale-x scale-y scale-z] vector for Manifolds."
  ([obj sv]
   #?(:clj (cond (manifold? obj) (.scale ^Manifold obj (DoubleVec3. (nth sv 0) (nth sv 1) (nth sv 2)))
                 (cross-section? obj) (.scale ^CrossSection obj (DoubleVec2. (nth sv 0) (nth sv 1)))
                 :else (throw (IllegalArgumentException. (str "Must be Manifold or CrossSection. Received:" (type obj)))))
      :cljs (.then obj
                   (fn [o]
                     (.scale o (clj->js sv)))))))

(defn bounds
  "Get the bounding `manifold3d.pub.Box` for Manifolds or `manifold3d.pub.Rect` for CrossSections."
  ([obj]
   #?(:clj (cond (manifold? obj) (.boundingBox ^Manifold obj)
                 (cross-section? obj) (.bounds ^CrossSection obj)
                 (satisfies? impl/ICSGConvertable obj) (bounds (impl/to-csg obj))
                 :else (throw (IllegalArgumentException. (str "Must be Manifold or CrossSection. Received: " (type obj)))))
      :cljs (update-manifold obj
                             (fn [man]
                               (.boundingBox man))))))

#?(:clj
   (defn get-height
     "Get the z-height of a manifold or y-height of a cross section."
     [obj]
     (let [x (impl/to-csg obj)]
       (cond (cross-section? obj) (-> ^CrossSection x (.bounds) (.Size) (.y))
             (manifold? obj) (-> ^Manifold x (.boundingBox) (.Size) (.z))))))

#?(:clj
   (defn get-properties
     ([^Manifold manifold]
      {:surface-area (double (.surfaceArea manifold))
       :volume (double (.volume manifold))})))

#?(:clj
   (defn area
     "Get the area of a cross section."
     [section]
     (.area ^CrossSection section)))

(defn trim-by-plane
  "Subtracts the halfspace defined by the `normal`, offset by `origin-offset` in the direction of the normal."
  ([manifold normal]
   (trim-by-plane manifold normal 0.0))
  ([manifold normal origin-offset]
   #?(:clj (let [[x y z] normal]
             (.trimByPlane ^Manifold manifold (DoubleVec3. x y z) origin-offset))
      :cljs (let [[x y z] normal]
              (update-manifold manifold
                               (fn [man]
                                 (.trimByPlane man #js [x y z] origin-offset)))))))

#?(:clj
   (defn project
     "Returns the 2D projection of `manifold` onto the xy plane."
     [manifold]
     ^CrossSection (.project ^Manifold (impl/to-csg manifold))))

#?(:clj
   (defn split-by-plane
     "Splits manifold two using the plane defined by the `normal`, offset by `origin-offset` in
  the direction of the normal. CLJ only."
     ([manifold normal]
      (split-by-plane manifold normal 0.0))
     ([manifold normal origin-offset]
      (let [[x y z] normal
            pair (.splitByPlane ^Manifold manifold (DoubleVec3. x y z) origin-offset)]
        [(.first pair) (.second pair)]))))

#?(:clj
   (defn split
     "Cuts `manifold` with the `cutter-manifold`. Returns vector of intersection and difference.
  More efficient than doing each operation separately. CLJ only."
     ([manifold cutter-manifold]
      (let [ret (.split ^Manifold manifold cutter-manifold)]
        [(.first ret) (.second ret)]))))

#?(:clj
   (defn frame
     "Create a column-major 4x3 matrix representing a 3D affine transformation (last row is always [0 0 0 1]) in the context of the Manifold library.

  This library takes the approach of \"interpreting\" the transform matrix as a coordinate frame. The first 3 columns represent
  the x,y, and z axes of the frame, and the last column represents the x,y,z position of the frame. Rotations of this frame
  are applied \"in place\" (i.e. relative to axes of the frame) using Rodrigues' rotation formula. Translations are applied relative to
  the frame. The unit length of each axis column is conserved under transformation.

  In practice, this often means the order of rotation and translation operations are \"flipped\" relative to Manifolds and CrossSections.
  i.e. (transform manifold (-> (frame 1) (rotate rv) (translate tv))) is equivalent to (-> manifold (translate tv) (rotate rv)).

  This approach makes it easy to define \"paths\" of frames, where the next frame in the path is a function of the previous frame.
  "
     ([]
      (DoubleMat3x4/IdentityMat))
     ([v]
      (.mul (DoubleMat3x4/IdentityMat) v))
     ([rx ry rz tr]
      (DoubleMat3x4. (DoubleVec3. (nth rx 0) (nth rx 1) (nth rx 2))
                     (DoubleVec3. (nth ry 0) (nth ry 1) (nth ry 2))
                     (DoubleVec3. (nth rz 0) (nth rz 1) (nth rz 2))
                     (DoubleVec3. (nth tr 0) (nth tr 1) (nth tr 2))))
     ([c0 c1 c2 c3
       c4 c5 c6 c7
       c8 c9 c10 c11]
      (DoubleMat3x4. c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11))))

#?(:clj
   (defn frame-2d
     "Create a matrix representing a 2D affine transformation."
     ([]
      (DoubleMat2x3/IdentityMat))
     ([x]
      (DoubleMat2x3. x))
     ([rx tr]
      (frame-2d rx [(nth rx 1) (- (nth rx 0))] tr))
     ([rx ry tr]
      (DoubleMat2x3. (DoubleVec2. (nth rx 0) (nth rx 1))
                     (DoubleVec2. (nth ry 0) (nth ry 1))
                     (DoubleVec2. (nth tr 0) (nth tr 1))))
     ([c0 c1 c2
       c3 c4 c5]
      (DoubleMat2x3. c0 c1 c2 c3 c4 c5))))

(defn translate
  "Translates a `Manifold` or `CrossSection` with the given vector."
  ([x tv]
   #?(:clj (impl/translate (if (satisfies? impl/ICSGConvertable x) (impl/to-csg x) x) tv)
      :cljs (update-manifold x (fn [o] (.translate o (clj->js tv)))))))

(defn tx
  "Translate along x axis."
  [x tx]
  #?(:clj (impl/translate (if (satisfies? impl/ICSGConvertable x) (impl/to-csg x) x) [tx 0 0])
     :cljs (update-manifold x (fn [o] (.translate o (clj->js [tx 0 0]))))))

(defn ty
  "Translate along y axis."
  [x ty]
  #?(:clj (impl/translate (if (satisfies? impl/ICSGConvertable x) (impl/to-csg x) x) [0 ty 0])
     :cljs (update-manifold x (fn [o] (.translate o (clj->js [0 ty 0]))))))

(defn tz
  "Translate along z axis."
  [x tz]
  #?(:clj (impl/translate (if (satisfies? impl/ICSGConvertable x) (impl/to-csg x) x) [0 0 tz])
     :cljs (update-manifold x (fn [o] (.translate o (clj->js [0 0 tz]))))))

(defn rotate
  "Rotates a `Manifold`, `CrossSection` or transform frame by the given rotation vector or number.

  Note that rotations of transform matrices are done 'in place', i.e. the [x y z] position is
  unchanged. You can interpret the rotation columns of the frame as the axes of a coordinate
  frame."
  ([x rv]
   #?(:clj (impl/rotate (if (satisfies? impl/ICSGConvertable x) (impl/to-csg x) x) rv)
      :cljs (update-manifold x (fn [o] (.rotate o (clj->js rv)))))))

#?(:clj
   (defn transform [x transform-matrix]
     (impl/transform (if (satisfies? impl/ICSGConvertable x) (impl/to-csg x) x) transform-matrix)))

#?(:clj
   (defn invert-frame
     "Create an \"inverse\" frame. This is not equivalent to matrix inversion but does
  have the property that (~= manifold (transform (transform manifold tf) (invert-transform tf)))"
     [m]
     (MatrixTransforms/InvertTransform m)))

#?(:clj
   (defn compose-frames
     "Compose transform frames. (~= (compose-frames (-> (frame 1) A) (-> (frame 1) B)) (-> (frame 1) A B)) for any sequences of
  transformations A and B."
     ([f g]
      (MatrixTransforms/CombineTransforms f g))
     ([f g & more]
      (reduce compose-frames (compose-frames f g) more))))

#?(:clj
   (defn center
     "Center Manifold or CrossSection along x, y, and/or z axes. `axes` is a set that specifies which axes to center along. Defaults
  to X and Y axes. CLJ only."
     ([obj]
      (center obj #{:x :y}))
     ([obj axes]
      (let [csg (impl/to-csg obj)
            axes (if (set? axes) axes (into #{} axes))
            bnds (.Center (bounds csg))
            tr (cond-> [(if (contains? axes :x) (- (.x bnds)) 0)
                        (if (contains? axes :y) (- (.y bnds)) 0)]
                 (manifold? csg) (conj (if (contains? axes :z) (- (.z bnds)) 0)))]
        (translate csg tr)))))

#?(:clj
   (defn snap
     "Snap a Manifold/CrossSection toa plane/line."
     ([obj]
      (snap obj #{:x}))
     ([obj axes]
      (let [csg (impl/to-csg obj)
            axes (if (set? axes) axes (into #{} axes))
            bnds (.Center (bounds csg))
            size (.Size (bounds csg))
            tr (cond-> [(if (contains? axes :x) (+ (- (.x bnds)) (/ (.x size) 2)) 0)
                        (if (contains? axes :y) (+ (- (.y bnds)) (/ (.y size) 2)) 0)]
                 (manifold? csg) (conj (if (contains? axes :z) (+ (- (.z bnds)) (/ (.z size) 2)) 0)))]
        (translate csg tr)))))

(defn get-mesh
  "Calculates and returns the Manifold's mesh. Note that most of the CSG work is done when running this function."
  [manifold]
  #?(:clj (.getMesh ^Manifold manifold)
     :cljs (update-manifold manifold (fn [man] (.getMesh man)))))

#?(:clj
   (defn get-mesh-gl
     "Calculates and returns the Manifold's mesh. Note that most of the CSG work is done when running this function."
     ([manifold]
      (.getMeshGL ^Manifold (impl/to-csg manifold) -1))
     ([manifold normal-idx]
      (.getMeshGL ^Manifold (impl/to-csg manifold) normal-idx))))

#?(:clj
   (defn- fill-rule->enum [fill-rule]
     (case fill-rule
       :even-odd 0
       :non-zero 1
       :positive 2
       :negative 3
       (throw (IllegalArgumentException. "fill-rule should be :even-odd, :non-zero, :positive or :negative")))))

(defn cross-section
  "Construct a Cross Section. `polygon-or-polygons` is either a sequence of [[x y] ...] points or sequence of
  [[[x y] ...] ...]. When providing multiple polygons, holes are automatically infered.
  `fill-rule` determines the algorithm used to interpret self-intersecting polygons
  (which should generally be avoided)."
  ([polygon-or-polygons]
   (cross-section polygon-or-polygons :non-zero))
  ([polygon-or-polygons fill-rule]
   #?(:clj
      (CrossSection. (if (coll? polygon-or-polygons)
                       (if (number? (ffirst polygon-or-polygons))
                         (SimplePolygon/FromArray (double-array (sequence cat polygon-or-polygons)))
                         (let [polys (Polygons.)]
                           (doseq [pts polygon-or-polygons]
                             (.pushBack polys (SimplePolygon/FromArray (double-array (sequence cat pts)))))
                           polys))
                       (impl/to-polygons polygon-or-polygons))
                     (fill-rule->enum fill-rule))
      :cljs (update-manifold *manifold-module*
                             (fn [module]
                               (.setup module)
                               (module.CrossSection. (clj->js polygon-or-polygons) fill-rule))))))

#?(:clj
   (defn slice
     "Returns the cross-section of `manifold` that intersects the xy plane."
     ([manifold]
      (cross-section ^Polygons (.slice ^Manifold (impl/to-csg manifold))))
     ([manifold height]
      (cross-section ^Polygons (.slice ^Manifold (impl/to-csg manifold) height)))))

#?(:clj
   (defn slices
     "Returns a vector of `n-slices` evenly spaced cross-sections between `bottom-z` and `top-z`."
     [manifold bottom-z top-z n-slices]
     (mapv cross-section (seq ^PolygonsVector (.slices ^Manifold manifold bottom-z top-z n-slices)))))

#?(:clj
   (defn text
     "Render `text` to a CrossSection using the provided `font-file`. `pixel-height` is a
  freetype size in pixels. Use scale-to-height to scale a text bounding box to a height in mm. `interp-res`
  specifies the numbers steps of interpolation of bezier curves. `fill-rule` determines how polygons are filled
  (should be left :even-odd in most cases)."
     ([font-file text-str]
      (text font-file text-str 10))
     ([font-file text-str pixel-height]
      (text font-file text-str pixel-height 10))
     ([font-file text-str pixel-height interp-res]
      (text font-file text-str pixel-height interp-res :even-odd))
     ([font-file text-str pixel-height interp-res fill-rule]
      (CrossSection/Text font-file text-str pixel-height interp-res (fill-rule->enum fill-rule)))))

(defn scale-to-height
  "Scale a CrossSection or Manifold to a given height. Height corresponds to y axis for CrossSection, z axis for Manifold."
  [obj height]
  (cond (cross-section? obj)
        (let [bnd (bounds ^CrossSection obj)
              size (.Size bnd)
              y (.y size)]
          (scale obj [(/ height y) (/ height y)]))
        (manifold? obj)
        (let [box (.boundingBox ^Manifold obj)
              size (.Size box)
              z (.z size)
              scale-factor (/ height z)]
          (scale obj [scale-factor scale-factor scale-factor]))))

#?(:clj
   (defn- three-point-circle-parameters
     "Calculate parameters for a three point circle."
     [p1 p2 p3]
     (let [v1 (u/vec-sub p2 p1)
           v2 (u/vec-sub p3 p1)

           a (u/vec-length v1)
           b (u/vec-length (u/vec-sub p3 p2))
           c (u/vec-length v2)
           [A _ _] (u/abc->ABC a b c)
           radius (/ a (* 2 (u/sin A)))
           D (u/acos (/ a (* 2 radius)))


           direction (u/cross-product-z v1 v2)

           center
           (-> (frame-2d (u/vec-normalize (u/vec-sub p2 p1)) p1)
               (rotate (* (- (/ direction (Math/abs direction)))
                          (if (> (- (+ A D) 0.0001) (/ Math/PI 2)) D  (- D))))
               (translate [radius 0])
               (.getColumn 2)
               (vec))]
       [radius center direction])))

(defn circle
  "Construct a circle of `circular-segments` edges centered on the xy plane."
  ([radius] (circle radius 0))
  ([radius circular-segments]
   #?(:clj (CrossSection/Circle radius circular-segments)
      :cljs (update-manifold *manifold-module*
                             (fn [module]
                               (.setup module)
                               (.circle module radius circular-segments)))))
  #?(:clj ([p1 p2 p3]
           (circle p1 p2 p3 0)))
  #?(:clj ([p1 p2 p3 circular-segments]
           (let [[radius center _] (three-point-circle-parameters p1 p2 p3)]
             (-> (circle radius circular-segments)
                 (translate center))))))

#?(:clj
   (defn three-point-arc-points
     "Returns points for an arc segment defined by three points."
     [p1 p2 p3 n-segments]
     (let [[radius center _] (three-point-circle-parameters p1 p2 p3)
           direction (u/cross-product-z
                      (u/vec-sub p1 center)
                      (u/vec-sub p2 center))
           op (if (pos? direction)
                u/angle-between-vectors-ccw
                u/angle-between-vectors-cw)
           p1-p2-angle (op (u/vec-sub p1 center)
                           (u/vec-sub p2 center))
           p2-p3-angle (op (u/vec-sub p2 center)
                           (u/vec-sub p3 center))
           curve-angle (+ p1-p2-angle p2-p3-angle)
           angle-increment (/ curve-angle n-segments)
           start-dir (u/vec-normalize (u/vec-sub p1 center))
           frame (frame-2d start-dir center)]
       (for [i (range (inc n-segments))]
         (-> frame
             (rotate (* i (if (neg? direction) -1 1) angle-increment))
             (translate [radius 0])
             (.getColumn 2)
             (vec))))))

#?(:clj
   (defn three-point-arc
     "Creates a cross-section from an arc segment defined by three points."
     [p1 p2 p3 n-segments]
     (cross-section (three-point-arc-points p1 p2 p3 n-segments))))


(defn square
  "Construct a square CrossSection. `center?` option centers the square in the xy plane."
  ([x y] (square x y false))
  ([x y center?]
   #?(:clj (CrossSection/Square (DoubleVec2. x y) center?)
      :cljs (update-manifold *manifold-module*
                             (fn [module]
                               (.setup module)
                               (.square module #js [x y] center?))))))

(defn offset
  "Offsets `section` by the given `delta`. Option `join-type` determines how edges are connected together.

  `miter-limit` specifies The maximum distance in multiples of delta that vertices
 can be offset from their original positions with before squaring is
 applied, <B>when the join type is Miter</B> (default is 2, which is the
  minimum allowed.

  See: http://www.angusj.com/clipper2/Docs/Units/Clipper.Offset/Classes/ClipperOffset/Properties/MiterLimit.htm

  `arc-tolerance` specifies the maximum acceptable imperfection for curves drawn
  (approximated with line segments) for Round joins (not relevant for other
  JoinTypes). By default (when undefined or =0), the allowable imprecision is
  scaled in inverse proportion to the offset delta."
  ([section delta]
   (offset section delta :square))
  ([section delta join-type]
   (offset section delta join-type 2.0))
  ([section delta join-type miter-limit]
   (offset section delta join-type miter-limit 0.0))
  ([section delta join-type miter-limit arc-tolerance]
   #?(:clj (.offset ^CrossSection section delta
                    (case join-type
                      :square 0
                      :round 1
                      :miter 2)
                    miter-limit arc-tolerance)
      :cljs (update-manifold section
                             (fn [x]
                               (.offset x delta (case join-type :square 0 :round 1 :miter 2)
                                        miter-limit arc-tolerance))))))

#?(:clj
   (defn to-polygons
     "Return the outer contours of cross-section as polygons. Returns iterable collections with
  DoubleVec2 objects as vertices. Use (.x vert) and (.y vert) to get x,y coordinates.
  This function has substantial JNI overhead. CLJ only."
     [cross-section]
     (impl/to-polygons cross-section)))

#?(:clj
   (defn status
     "Get manifold error status."
     [manifold]
     (case (.status ^Manifold manifold)
       0 :NoError
       1 :NonFiniteVertex
       2 :NotManifold
       3 :VertexOutOfBounds
       4 :PropertiesWrongLength
       5 :MissingPositionProperties
       6 :MergeVectorsDifferentLengths
       7 :MergeIndexOutOfBounds
       8 :TransformWrongLength
       9 :RunIndexWrongLength
       10 :FaceIDWrongLength
       11 :InvalidConstruction)))

#?(:clj
   (defn color
     "Apply color properties to a manifold's. `color` is a normalized vector of [r g b alpha].
  The color value is applied to each vertex and is preserved under boolean operations, as long as all
  combined Manifolds have color channels defined. Optional `prop-index` specifies the offset of the color
  property indices. For exported meshes to retain color peroperties, you must use `get-mesh-gl` and not
  `get-mesh`.

  Note, to export maniofolds with color propertes, you need to specify the color property index
  in the material, e.x.  (material :roughness 0.0 :metalness 0.0 :color-channels [3 4 5 6])"
     ([man rgba]
      (color ^Manifold man rgba 3))
     ([man rgba prop-index]
      (let [[r g b a] rgba]
        (MeshUtils/ColorVertices ^Manifold man (DoubleVec4. r g b a) prop-index)))))


#?(:clj
   (defn- >polygons [x]
     (cond (satisfies? impl/IToPolygons x) (to-polygons x)
           (sequential? x) (let [pv (Polygons.)]
                             (doseq [poly (if (number? (ffirst x)) [x] x)]
                               (.pushBack pv (SimplePolygon/FromBuffer (double-vec2-sequence-to-native-double-buffer poly))))
                             pv))))
#?(:clj
   (defn- loft-algorithm->enum [name]
     (case name
       :eager-nearest-neighbor MeshUtils$LoftAlgorithm/EagerNearestNeighbor
       :isomorphic MeshUtils$LoftAlgorithm/Isomorphic)))

#?(:clj
   (defn loft
     "Loft between cross sections transformed by the associated 3D transform frames.
  If a single cross-section is provided, lofts between the same cross section at each frame position.
  It also accepts a sequence of cross-sections, or a \"decomposed\" sequence of cross-sections,
  i.e. a vector of vectors of polygons (which are vectors of points). Note there is a cannonical polygon
  set for every cross section, with holes encoded by winding order.

  There is also an optional `algorithm` argument. Options are :eager-nearest-neighbor (default) and
  :isomorphic. :eager-nearest-neighbor constructs edges by eagerly adding the edge of minimum distance as it
  zips around consecutive polygons. It can handle many cases of many-to-one and one-to-many vertex mappings.
  However, all lofted cross sections must decompose into equal numbers of polygons. :isomorphic is the simplest and
  maps vertices of consecutive polygons one-to-one. It requires that the order and number of polygons and polygon
  vertices is equivalent for all cross-sections. It is slightly faster.

  Note loft is *not* guaranteed to be well-defined for all input combinations. Users should roughy understand
  how the the \"skinning\" of cross-sections works for a given algorithm type.

  ex.
  (let [c (difference (square 10 10 true) (square 8 8 true))]
    (loft [c (scale c [1.5 1.5]) c]
          [(frame 1)
           (-> (frame 1) (translate [0 0 15]))
           (-> (frame 1) (translate [0 0 30]))]))

  (loft (difference (square 10 10 true) (square 8 8 true))
        (reductions
         (fn [f [x y]]
           (-> f (translate [x y 30])))
         (frame 1)
         [[20 0]
          [0 20]
          [20 0]]))

  It also has a single arity operation that accepts a vector of {:cross-section ... :frame ...} maps.
  It will automatically use the \"lastest\" cross-section if one or more is missing. :frame must be
  defined in each map. First map can optionally include a :algorithm keyword to specify the lofting
  algorithm.

  (loft
   (reductions
    (fn [m _]
      (assoc m
             :frame (-> (:frame m)
                        (MatrixTransforms/Yaw (/ 0.2 2))
                        (MatrixTransforms/Translate (DoubleVec3. 0 0 10))
                        (MatrixTransforms/Yaw (/ 0.2 2)))))
    {:cross-section (difference (square 4 4 true)
                                (square 2 2 true))
     :algorithm :earger-nearest-neighbor ;; Optional algorithm specifier in first map.
     :frame (frame 1)}
  (cons (rem (* 2 Math/PI) 0.2) (range (quot (* 2 Math/PI) 0.2)))))"
     ([loft-segments]
      (let [cv (PolygonsVector.)
            fv (DoubleMat3x4Vector.)
            first-seg (first loft-segments)
            last-cross-section (:cross-section first-seg)
            last-frame (or (:frame first-seg) (frame 1))
            algorithm (:algorithm first-seg :eager-nearest-neighbor)]
        (when (nil? last-cross-section)
          (throw (IllegalArgumentException. "First loft segment must contain :cross-section")))
        (loop [last-cross-section last-cross-section
               last-frame last-frame
               [{:keys [frame frame-fn cross-section cross-section-fn] :as segment} & more] loft-segments]
          (cond (nil? segment) (MeshUtils/Loft cv fv (loft-algorithm->enum algorithm))
                :else (let [c (if cross-section-fn
                                (cross-section-fn last-cross-section)
                                (or cross-section last-cross-section))
                            f (if frame-fn
                                (frame-fn last-frame)
                                (or frame last-frame))]
                        (.pushBack cv (>polygons c))
                        (.pushBack fv f)
                        (recur c f more))))))
     ([cross-sections frames]
      (loft cross-sections frames :eager-nearest-neighbor))
     ([cross-sections frames algorithm]
      (let [sections (if (cross-section? cross-sections)
                       cross-sections
                       (let [cv (PolygonsVector.)]
                         (when-not (= (count cross-sections) (count frames))
                           (throw (IllegalArgumentException. "cross-sections and frames must be same length.")))
                         (doseq [c cross-sections]
                           (.pushBack cv (>polygons c)))
                         cv))
            tv (DoubleMat3x4Vector.)]
        (doseq [^DoubleMat3x4 t frames]
          (.pushBack tv t))
        (MeshUtils/Loft sections ^DoubleMat3x4Vector tv (loft-algorithm->enum algorithm))))))


(defprotocol IHalfEdge
  (is-forward [this]))

(defrecord Halfedge [start-vert end-vert paired-halfedge face]
  IHalfEdge
  (is-forward [this] (< start-vert end-vert)))

#?(:clj
   (defn get-halfedges
     "Get halfedges of `man`."
     [man]
     (let [^ints halfedges (.toIntArray (.getHalfedges ^Manifold man))]
       (loop [idx 0
              ret (transient [])]
         (if (>= idx (alength halfedges))
           (persistent! ret)
           (recur (+ idx 4)
                  (conj! ret (Halfedge. (aget halfedges idx)
                                        (aget halfedges (+ idx 1))
                                        (aget halfedges (+ idx 2))
                                        (aget halfedges (+ idx 3))))))))))

#?(:clj
   (defn get-vertices
     "Get vertices of `man`."
     [man]
     (let [^floats vertices (.toFloatArray (.getVertices ^Manifold man))]
       (loop [idx 0
              ret (transient [])]
         (if (>= idx (alength vertices))
           (persistent! ret)
           (recur
            (+ idx 3)
            (conj! ret [(aget vertices idx)
                        (aget vertices (+ idx 1))
                        (aget vertices (+ idx 2))])))))))

#?(:clj
   (defn get-triangles
     "Get triangles of `man`."
     [man]
     (let [^ints triangles (.toIntArray (.getTriangles ^Manifold man))]
       (loop [idx 0
              ret (transient [])]
         (if (>= idx (alength triangles))
           (persistent! ret)
           (recur
            (+ idx 3)
            (conj! ret [(aget triangles idx)
                        (aget triangles (+ idx 1))
                        (aget triangles (+ idx 2))])))))))

#?(:clj
   (defn get-face-normals [man]
     (let [face-normals (.toFloatArray (.getFaceNormals ^Manifold man))]
       (into [] (partition-all 3) face-normals))))

#?(:clj
   (defn simplify
     "Remove vertices from contours in `cross-section` that are less than the specified distance
  `epsilon` from an imaginary line that passes through its two adjacent vertices. Near-duplicates
  and colinear points will be removed. It is recommended to apply this function after `offset`, especially when
  offseting with join-type = :miter. CLJ only."
     [cross-section epsilon]
     (.simplify ^CrossSection (impl/to-csg cross-section) epsilon)))

#?(:clj
   (defn material
     "Create a material for use with export-mesh when exporting as .glb format.

  :roughness - (float) higher for more surface roughness.
  :metalness - (float) higher for more metalic (shiny) surface.
  :color - ([r g b alpha]) a uniform color for the mesh. Alpha determines opaqueness.
  :vert-color - ([[r g b alpha] ...]) a color attribute for each vertex. Length must match vert-pos of the mesh.
  :normal-channels - ([[i j k] ...]) For MeshGL exports (not yet supported). Indices where the normal channels can be found.
  :color-channels - ([[i j k l] ...]) For MeshGL exports (not yet supported). Indicies where color channels can be found."
     [& {:keys [roughness metalness color alpha normal-idx color-idx alpha-idx]}]
     (cond-> (Material.)
       roughness (doto (.roughness roughness))
       metalness (doto (.metalness metalness))
       color (doto (.color (DoubleVec3. (nth color 0) (nth color 1) (nth color 2))))
       alpha (doto (.alpha alpha))
       normal-idx (doto (.normalIdx normal-idx))
       color-idx (doto (.colorIdx color-idx))
       alpha-idx (doto (.alphaIdx alpha-idx)))))

#?(:clj
   (defn export-mesh
     "Exports a mesh of specified format based on the file extension. Supports .stl, .glb, .3mf, .3ds, and .obj formats
  (possibly others as well). It's important to use a binary format for large models."
     [mesh filename & {:keys [faceted material]}]
     (MeshIO/ExportMesh filename
                        mesh
                        (cond-> ^ExportOptions (ExportOptions.)
                          faceted  (doto (.faceted true))
                          material (doto (.material material))))))

#?(:clj
   (defn import-mesh
     "Imports a mesh from a file. Supports .stl, .glb, .3mf, .3ds, and .obj formats (possibly others as well)."
     ([filename]
      (MeshIO/ImportMesh filename false))
     ([filename force-cleanup?]
      (MeshIO/ImportMesh filename force-cleanup?))))

(defn -main [& args])


(comment

  (-> (difference (color (sphere 100 100) [1 0 0 1])
                  (color (cube 100 100 100 false) [0 0 1 1]))
      (get-mesh-gl)
      (export-mesh "test.glb" :material (material :roughness 0.0 :metalness 0.0 :color-idx 0 :alpha-idx 3)))

  (slices (sphere 100 100) -50 50 20)

  (-> (slice (sphere 100 100))
      (impl/to-polygons)
      (translate [100 0])
      (revolve 100 180))

  (-> (sphere 100 10)
      (extrude 10))

  )
