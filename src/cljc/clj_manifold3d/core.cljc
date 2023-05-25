(ns clj-manifold3d.core
  "This library defines a thin wrapper over Manifold for clojure and clojurescript.
  Refer to the original library for complete documentation.

  This library aspires to achieve code capatibility between Clojure and ClojureScript so that models
  build in the more friendly Java environment can be shared and distributed in the javascript
  environment. However, there are challenges in the way the Manifold js library is provided
  as a promise. To (mostly) support this, this library elects to accept promises at the API level.
  Working with promises can be pretty annoying, especially without a type system that supports
  them well. For this reason, the CLJS API generally also works on non-promise objects.

  I've also decided to prefer radians to degrees across the API, even though Manifold uses degrees
  and degrees actually have generally better performance and accuracy than radians.
  But most exsting OpenSCAD code uses radians,"
  #?(:clj (:import
           [manifold3d Manifold]
           [manifold3d.pub DoubleMesh SmoothnessVector Smoothness SimplePolygon]
           [manifold3d.manifold CrossSection MeshIO ExportOptions]
           [manifold3d MeshUtils]
           [manifold3d.glm DoubleVec3 DoubleVec2 DoubleMat4x3 DoubleMat3x2
            DoubleVec3Vector DoubleVec4Vector IntegerVec3Vector MatrixTransforms]
           [java.nio ByteBuffer ByteOrder DoubleBuffer IntBuffer]))
  #?(:clj
     (:require
      [clj-manifold3d.impl :as impl])
     :cljs
     (:require
      ["/clj_manifold3d/manifold" :as manifold3d])))

#?(:cljs
   (def ^:dynamic *manifold-module* (manifold3d)))

#?(:cljs
   (defn update-manifold [manifold f]
     (if (promise? manifold)
       (.then manifold f)
       (f manifold))))

#_(set! *warn-on-reflection* true)

#?(:clj (defn- double-vec3-sequence-to-native-double-buffer
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

#?(:clj (defn- double-vec4-sequence-to-native-double-buffer
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

#?(:clj (defn- integer-vec3-sequence-to-native-integer-buffer
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

#?(:clj (defn mesh
          "Convenience function to create a mesh from sequences as efficiently as reasonably possible in Clojure. :vert-pos and :tri-verts should be included together. Others will be computed later if not provided.
  For maximum java performance use FromBuffer constructors directly and use natively ordered structure that can provide java.nio buffers. If necessary, write meshing alorthims in c++ using
  GLM directly and bind to them."
          [& {:keys [tri-verts vert-pos vert-normal halfedge-tangent]}]
          (cond-> (DoubleMesh.)
            tri-verts (doto (.triVerts (IntegerVec3Vector/FromBuffer (integer-vec3-sequence-to-native-integer-buffer tri-verts))))
            vert-pos (doto (.vertPos (DoubleVec3Vector/FromBuffer (double-vec3-sequence-to-native-double-buffer vert-pos))))
            vert-normal (doto (.vertNormal (DoubleVec3Vector/FromBuffer (double-vec3-sequence-to-native-double-buffer vert-normal))))
            halfedge-tangent (doto (.halfedgeTangent (DoubleVec4Vector/FromBuffer (double-vec4-sequence-to-native-double-buffer halfedge-tangent)))))))

(defn manifold
  "Creates a `Manifold` ."
  ([] #?(:clj (Manifold.)))
  ([mesh]
   #?(:clj (Manifold. ^DoubleMesh mesh)
      :cljs (update-manifold *manifold-module*
                             (fn [mod]
                               (.setup mod)
                               (mod.Manifold. (mod.Mesh. (clj->js mesh))))))))

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
   #?(:clj (Manifold/Extrude cross-section height n-divisions twist-degrees (DoubleVec2. (nth scale-top 0) (nth scale-top 1)))
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
   #?(:clj (Manifold/Smooth ^DoubleMesh mesh
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
   #?(:clj (Manifold/Revolve cross-section circular-segments degrees)
      :cljs (.then (js/Promise.all #js [*manifold-module* cross-section])
                   (fn [[module cross-section]]
                     (.revolve module cross-section circular-segments degrees))))))

(defn hull
  "Takes two or more `Manifolds` or `CrossSections` and returns their convex hull."
  ([a b]
   #?(:clj ^Manifold (.ConvexHull ^Manifold a ^Manifold b)
      :cljs (.then (js/Promise.all #js [a b])
                   (fn [[a b]]
                     (.convexHull a b)))))
  ([a b & more]
   (reduce hull (hull a b) more)))

#?(:cljs
   (defn warp
     "Returns a new `Manifold` with `warp` function applied to each vertex. CLJS only."
     [manifold func]
     (update-manifold manifold (fn [man] (.warp man func)))))

(defn manifold?
  "Returns true if `x` is a `Manifold`."
  [x]
  #?(:clj (instance? Manifold x)
     :cljs ((p/let [mod *manifold-module*
                    i x]
              (instance? mod.Manifold i)))))

(defn cross-section?
  "Returns true if `x` is a `CrossSection`."
  [x]
  #?(:clj (instance? CrossSection x)
     :cljs (p/let [mod *manifold-module*
                   i x]
             (instance? mod.CrossSection i))))

(defn union
  "Returns the union of two or more `Manifold`s or `CrossSections`."
  ([a] a)
  ([a b] #?(:clj (impl/union a b)
            :cljs (.then (js/Promise.all #js [*manifold-module* a b])
                         (fn [[module & args]]
                           (.setup module)
                           (.union module args)))))
  ([a b & more]
   #?(:clj (reduce union (union a b) more)
      :cljs (.then (js/Promise.all (list* *manifold-module* a b more))
                   (fn [[module & args]]
                     (.setup module)
                     (.union module args))))))

(defn difference
  "Returns the difference between two or more `Manifold`s or `CrossSection`s."
  ([a] a)
  ([a b] #?(:clj (impl/difference a b)
            :cljs (.then (js/Promise.all #js [*manifold-module* a b])
                         (fn [[module & args]]
                           (.setup module)
                           (.difference module args)))))
  ([a b & more]
   #?(:clj (reduce difference (difference a b) more)
      :cljs (.then (js/Promise.all (list* *manifold-module* a b more))
                   (fn [[module & args]]
                     (.setup module)
                     (.difference module args))))))

(defn intersection
   "Returns the intersection of two or more `Manifold`s or `CrossSection`s."
  ([a] a)
  ([a b] #?(:clj (impl/intersection a b)
            :cljs (.then (js/Promise.all #js [*manifold-module* a b])
                         (fn [[module & args]] (.intersection module args)))))
  ([a b & more]
   #?(:clj (reduce intersection (intersection a b) more)
      :cljs (.then (js/Promise.all (list* *manifold-module* a b more))
                   (fn [[module & args]]
                     (.setup module)
                     (.intersection module args))))))

(defn scale
  ([manifold [x y z]]
   #?(:clj (.Scale ^Manifold manifold (DoubleVec3. x y z))
      :cljs (.then manifold
                   (fn [man]
                     (.scale man #js [x y z]))))))

(defn bounding-box
  ([manifold]
   #?(:clj (.boundingBox ^Manifold manifold)
      :cljs (update-manifold manifold
                             (fn [man]
                               (.boundingBox man))))))

#?(:clj (defn get-properties
          ([manifold]
           (let [props (.GetProperties ^Manifold manifold)]
             {:surface-area (.surfaceArea props)
              :volume (.volume props)}))))

#_(keys (.volume (get-properties (cube 10 10 10 true))))

(defn trim-by-plane
  "Subtracts the halfspace defined by the `normal`, offset by `origin-offset` in the direction of the normal."
  ([manifold normal]
   (trim-by-plane manifold normal 0.0))
  ([manifold normal origin-offset]
   #?(:clj (let [[x y z] normal]
             (.trimByPlane ^Manifold manifold (DoubleVec3. x y z) origin-offset))
      :cljs (let [[x y z] normal]
              (.trimByPlane manifold #js [x y z] origin-offset)))))

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
      #?(:clj (let [ret (.split ^Manifold manifold cutter-manifold)]
                [(.first ret) (.second ret)])))))

#?(:clj
   (defn frame
     "Create a column-major 4x3 matrix representing an affine transformation (last row is always [0 0 0 1]) in the context of the Manifold library.

  This library takes the approach of \"interpreting\" the transform matrix as a coordinate frame. The first 3 columns represent
  the x,y, and z axes of the frame, and the last column represents the x,y,z position of the frame. Rotations of this frame
  are applied \"in place\" (i.e. relative to axes of the frame) using Rodrigues' rotation formula. Translations are applied relative to
  the frame. Note that the unit length of each axis column is conserved under rotation.

  In practice, this often means the order of rotation translation operations are \"flipped\" relative to Manifolds and CrossSections.
  i.e. (transform manifold (-> (frame 1) (rotate rv) (translate tv))) is equivalent to (-> manifold (translate tv) (rotate rv)).

  This approach makes it easy to defined \"paths\" of frames, where the next frame in the path is a function of the previous frame.
  "
     ([]
      (DoubleMat4x3.))
     ([v]
      (DoubleMat4x3. v))
     ([rx ry rz tr]
      (DoubleMat4x3. (DoubleVec3. (nth rx 0) (nth rx 1) (nth rx 2))
                     (DoubleVec3. (nth ry 0) (nth ry 1) (nth ry 2))
                     (DoubleVec3. (nth rz 0) (nth rz 1) (nth rz 2))
                     (DoubleVec3. (nth tr 0) (nth tr 1) (nth tr 2))))
     ([c0 c1 c2 c3
       c4 c5 c6 c7
       c8 c9 c10 c11]
      (DoubleMat4x3. c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11))))

(defn translate
  "Translates a `Manifold` or `CrossSection` with the given vector."
  ([x tv]
   #?(:clj (impl/translate x tv)
      :cljs (update-manifold x (fn [o] (.translate o (clj->js tv)))))))

(defn rotate
  "Rotates a `Manifold`, `CrossSection` or transform frame by the given rotation vector.

  Note that rotations of transform matrices are done 'in place', i.e. the [x y z] position is
  unchanged. You can interpret the rotation columns of the frame as the axes of a coordinate
  frame."
  ([x rv]
   #?(:clj (impl/rotate x rv)
      :cljs (update-manifold x (fn [o] (.rotate o (clj->js rv)))))))

(defn transform [x transform-matrix]
  #?(:clj (impl/transform x transform-matrix)
     :cljs (update-manifold x (fn [o] (.rotate o (clj->js transform-matrix))))))

(defn get-mesh [manifold]
  #?(:clj (.GetMesh ^Manifold manifold)
     :cljs (update-manifold manifold (fn [man] (.getMesh man)))))

(defn cross-section
  "Construct a Cross Section. `polygon` is a sequence of [[x y] ...] points ordered CCW.
  `fill-rule` determines the algorithm used to interpret self-intersecting polygons
  (which should generally be avoided)."
  ([polygon]
   (cross-section polygon :non-zero))
  ([polygon fill-rule]
   #?(:clj (CrossSection. (SimplePolygon/FromArray (double-array (sequence cat polygon)))
                          (case fill-rule
                            :non-zero 0
                            :positive 1
                            :negative 2
                            (throw (IllegalArgumentException. "fill-rule should be :non-zero, :positive or :negative"))))
      :cljs (update-manifold *manifold-module*
                             (fn [module]
                               (.setup module)
                               (module.CrossSection. (clj->js polygon) fill-rule))))))

(defn circle
  "Construct a circle of `circular-segments` edges centered on the xy plane."
  ([radius] (circle radius 0))
  ([radius circular-segments]
   #?(:clj (CrossSection/Circle radius circular-segments)
      :cljs (update-manifold *manifold-module*
                             (fn [module]
                               (.setup module)
                               (.circle module radius circular-segments))))))

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
   #?(:clj (.Offset ^CrossSection section delta
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
   (defn export-mesh
     "Exports a mesh of specified format based on the file extension. Supports .stl, .glb and .obj formats
  (possibly others as well)."
     [mesh filename  & {:keys [faceted]}]
     (MeshIO/ExportMesh filename
                        mesh
                        (cond-> (ExportOptions.)
                          faceted (.faceted true)))))

#?(:clj
   (defn import-mesh
     "Imports a mesh from a file. Supports .stl, .glb and .obj formats (possibly others as well)."
     ([filename]
      (MeshIO/ImportMesh filename false))
     ([filename force-cleanup?]
      (MeshIO/ImportMesh filename force-cleanup?))))
