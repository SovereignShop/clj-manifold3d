(ns clj-manifold3d.core
  "This library defines a wrapper over Manifold for clojure and clojurescript.
  Refer to the original library for complete documentation.

  This library aspires to achieve code capatibility between Clojure and ClojureScript so that models
  build in the more friendly Java environment can be shared and played with or parameterized in the javascript
  environment. However, there are challenges in the way the Manifold js library is provided
  as a promise. To (mostly) support this, this library elects to accept promises at the API level.
  Working with promises can be pretty annoying, especially without a type system that supports
  them well. For this reason, the CLJS API generally also works on non-promise objects."
  #?(:clj (:import
           [manifold3d Manifold MeshUtils ManifoldVector]
           [manifold3d.pub DoubleMesh SmoothnessVector Smoothness SimplePolygon OpType]
           [manifold3d.manifold CrossSection CrossSectionVector Material ExportOptions MeshIO]
           [manifold3d.glm DoubleVec3 DoubleVec2 DoubleMat4x3 DoubleMat3x2 IntegerVec4Vector
            DoubleVec3Vector DoubleVec4Vector IntegerVec3Vector IntegerVec3 MatrixTransforms DoubleVec4]
           [java.nio ByteBuffer ByteOrder DoubleBuffer IntBuffer]))
  #?(:clj
     (:require
      [clj-manifold3d.impl :as impl])
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
   (defn- integer-vec4-sequence-to-native-integer-buffer
     "Maps a sequence of 3-sequences to a flat native-ordered row-major integer buffer."
     [col]
     (let [buf (-> (ByteBuffer/allocateDirect (* (count col) 3 Integer/BYTES))
                   (.order (ByteOrder/nativeOrder))
                   (.asIntBuffer))]
       (doseq [[^int a ^int b ^int c ^int d] col]
         (.put buf a)
         (.put buf b)
         (.put buf c)
         (.put buf d))
       (.flip buf))))

(defn manifold?
  "Returns true if `x` is a `Manifold`."
  [x]
  #?(:clj (instance? Manifold x)
     :cljs (p/let [mod *manifold-module*
                   i x]
             (instance? mod.Manifold i))))

(defn cross-section?
  "Returns true if `x` is a `CrossSection`."
  [x]
  #?(:clj (instance? CrossSection x)
     :cljs (p/let [mod *manifold-module*
                   i x]
             (instance? mod.CrossSection i))))

#?(:clj
   (defn mesh
     "Convenience function to create a mesh from sequences as efficiently as reasonably possible in Clojure. :vert-pos and :tri-verts should be included together. Others will be computed later if not provided.
  For maximum java performance use FromBuffer constructors directly and use natively ordered structures that can provide java.nio buffers. If necessary, write meshing alorthims in c++ using
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

(defn mirror
  "Mirrors manifold over the plane desribed by the normal."
  ([manifold normal]
   #?(:clj (.mirror ^Manifold manifold (DoubleVec3. (nth normal 0) (nth normal 1) (nth normal 2)))
      :cljs (update-manifold manifold
                             (fn [man]
                               (.mirror man (clj->js normal)))))))

#?(:clj
   (defn refine
     "partitions every edge of the Manifold into `n` edges of equal length. CLJ only."
     ([manifold n]
      (.refine ^Manifold manifold n))))

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

#?(:cljs
   (defn warp
     "Returns a new `Manifold` with `warp` function applied to each vertex. CLJS only."
     [manifold func]
     (update-manifold manifold (fn [man] (.warp man func)))))

(defn hull
  "Takes two or more Manifolds or CrossSection and returns their convex hull."
  ([a b]
   #?(:clj (cond (manifold? a) ^Manifold (.convexHull ^Manifold a ^Manifold b)
                 (cross-section? a) ^CrossSection (.convexHull ^CrossSection a ^CrossSection b)
                 :else (throw (IllegalArgumentException. (str "Must be Manifold or CrossSection. Recieved: " (type a)))))
      :cljs (.then (js/Promise.all #js [a b])
                   (fn [[a b]]
                     (.convexHull a b)))))
  ([a b & more]
   (reduce hull (hull a b) more)))

(defn union
  "Returns the union of two or more Manifolds or CrossSections."
  ([a] a)
  ([a b]
   #?(:clj (impl/union a b)
      :cljs (.then (js/Promise.all #js [*manifold-module* a b])
                   (fn [[module & args]]
                     (.setup module)
                     (.union module args)))))
  ([a b & more]
   #?(:clj (impl/batch-boolean a (cons b more) OpType/Add)
      :cljs (.then (js/Promise.all (list* *manifold-module* a b more))
                   (fn [[module & args]]
                     (.setup module)
                     (.union module args))))))

(defn difference
  "Returns the difference between two or more `Manifold`s or `CrossSection`s."
  ([a] a)
  ([a b]
   #?(:clj (impl/difference a b)
      :cljs (.then (js/Promise.all #js [*manifold-module* a b])
                   (fn [[module & args]]
                     (.setup module)
                     (.difference module args)))))
  ([a b & more]
   #?(:clj (impl/batch-boolean a (cons b more) OpType/Subtract)
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
   #?(:clj (impl/batch-boolean a (cons b more) OpType/Intersect)
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
  ([obj sv]
   #?(:clj (cond (manifold? obj) (.scale ^Manifold manifold (DoubleVec3. (nth sv 0) (nth sv 1) (nth sv 2)))
                 (cross-section? obj) (.scale ^CrossSection obj (DoubleVec2. (nth sv 0) (nth sv 1)))
                 :else (throw (IllegalArgumentException. (str "Must be Manifold or CrossSection. Received:" (type obj)))))
      :cljs (.then obj
                   (fn [o]
                     (.scale o (clj->js sv)))))))

(defn bounding-box
  ([manifold]
   #?(:clj (.boundingBox ^Manifold manifold)
      :cljs (update-manifold manifold
                             (fn [man]
                               (.boundingBox man))))))

#?(:clj
   (defn get-properties
     ([manifold]
      (let [props (.getProperties ^Manifold manifold)]
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
              (update-manifold manifold
                               (fn [man]
                                 (.trimByPlane man #js [x y z] origin-offset)))))))

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

#?(:clj
   (defn frame-2d
     "Create a matrix representing a 2D affine transformation."
     ([]
      (DoubleMat3x2.))
     ([x]
      (DoubleMat3x2. x))
     ([rx ry tr]
      (DoubleMat3x2. (DoubleVec2. (nth rx 0) (nth rx 1))
                     (DoubleVec2. (nth ry 0) (nth ry 1))
                     (DoubleVec2. (nth tr 0) (nth tr 1))))
     ([c0 c1 c2
       c3 c4 c5]
      (DoubleMat3x2. c0 c1 c2 c3 c4 c5))))

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

#?(:clj
   (defn transform [x transform-matrix]
     (impl/transform x transform-matrix)))

(defn get-mesh [manifold]
  #?(:clj (.getMesh ^Manifold manifold)
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
   (defn material
     "Create a material for use with export-mesh when exporting as .glb format.

  :roughness - (float) higher for more surface roughness.
  :metalness - (float) higher for more metalic (shiny) surface.
  :color - ([r g b alpha]) a uniform color for the mesh. Alpha determines opaqueness.
  :vert-color - ([[r g b alpha] ...]) a color attribute for each vertex. Length must match vert-pos of the mesh.
  :normal-channels - ([[i j k] ...]) For MeshGL exports (not yet supported). Indices where the normal channels can be found.
  :color-channels - ([[i j k l] ...]) For MeshGL exports (not yet supported). Indicies where color channels can be found."
     [& {:keys [roughness metalness color vert-color normal-channels color-channels]}]
     (cond-> (Material.)
       roughness (doto (.roughness roughness))
       metalness (doto (.metalness metalness))
       color (doto (.color (DoubleVec4. (nth color 0) (nth color 1) (nth color 2) (nth color 3))))
       vert-color (doto (.vertColor ^DoubleVec4Vector
                                    (if (seq? vert-color)
                                      (double-vec4-sequence-to-native-double-buffer vert-color)
                                      vert-color)))
       normal-channels (doto (.normalChannels ^IntegerVec3Vector
                                              (if (seq? normal-channels)
                                                (integer-vec3-sequence-to-native-integer-buffer normal-channels)
                                                normal-channels)))
       color-channels (doto (.colorChannels ^IntegerVec4Vector
                                            (if (seq? color-channels)
                                              (integer-vec4-sequence-to-native-integer-buffer color-channels)
                                              :color-channels))))))

#?(:clj
   (defn export-mesh
     "Exports a mesh of specified format based on the file extension. Supports .stl, .glb, .3mf and .obj formats
  (possibly others as well)."
     [mesh filename & {:keys [faceted material]}]
     (MeshIO/ExportMesh filename
                        mesh
                        (cond-> ^ExportOptions (ExportOptions.)
                          faceted  (doto (.faceted true))
                          material (doto (.material material))))))

#?(:clj
   (defn import-mesh
     "Imports a mesh from a file. Supports .stl, .glb, .3mf, and .obj formats (possibly others as well)."
     ([filename]
      (MeshIO/ImportMesh filename false))
     ([filename force-cleanup?]
      (MeshIO/ImportMesh filename force-cleanup?))))
