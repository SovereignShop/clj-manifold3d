(ns clj-manifold3d.core
  (:require
   ["/clj_manifold3d/manifold" :as manifold3d]))

(def ^:dynamic *manifold-module* (manifold3d))

(defn promise? [x]
  (instance? js/Promise x))

(defn update-manifold [manifold f]
  (if (promise? manifold)
    (.then manifold f)
    (f manifold)))

(defn manifold
  ([]
   (manifold #js {}))
  ([mesh]
   (update-manifold *manifold-module*
                    (fn [mod]
                      (.setup mod)
                      (mod.Manifold. (mod.Mesh. (clj->js mesh)))))))

(defn square
  ([x y] (square x y true))
  ([x y center?]
   (update-manifold *manifold-module*
                    (fn [module]
                      (.setup module)
                      (.square module #js [x y] center?)))))

(defn circle
  ([radius] (circle radius 0))
  ([radius circular-segments]
   (update-manifold *manifold-module*
                    (fn [module]
                      (.setup module)
                      (.circle module radius circular-segments)))))

(defn cross-section
  [pts]
  (update-manifold *manifold-module*
                   (fn [module]
                     (.setup module)
                     (module.CrossSection. (clj->js pts)))))

(defn revolve
  ([section]
   (revolve section 0))
  ([section circular-segments]
   (revolve section circular-segments 360))
  ([section circular-segments degrees]
   (.then (js/Promise.all #js [*manifold-module* section])
          (fn [[module cross-section]]
            (.revolve module cross-section circular-segments degrees)))))

(defn warp
  [manifold func]
  (update-manifold manifold (fn [man] (.warp man func))))

(defn translate
  [obj & vec]
  (update-manifold obj (fn [o] (.translate o (clj->js vec)))))

(defn rotate
  [obj vec]
  (update-manifold obj (fn [o] (.rotate o (clj->js vec)))))

(defn scale
  [manifold vec]
  (update-manifold manifold (fn [man] (.scale man (clj->js vec)))))

(defn mirror
  [manifold vec]
  (update-manifold manifold (fn [man] (.mirror man (clj->js vec)))))

(defn trim-by-plane
  [manifold normal offset]
  (update-manifold manifold (fn [man] (.trimByPlane man (clj->js normal) offset))))

(defn get-mesh
  [manifold]
  (update-manifold manifold (fn [man] (.getMesh man))))

(defn decompose
  [manifold]
  (update-manifold manifold (fn [man] (.decompose man))))

(defn get-curvature
  [manifold]
  (update-manifold manifold (fn [man] (.getCurvature man))))

(defn cube
  ([xyz center?]
   (update-manifold *manifold-module*
                    (fn [module]
                      (.setup module)
                      (.cube module (clj->js xyz) center?))))
  ([x y z]
   (update-manifold *manifold-module*
                    (fn [module]
                      (.setup module)
                      (.cube module #js [x y z]))))
  ([x y z center?]
   (cube #js [x y z] center?)))

(defn cylinder
  ([height radius-low]
   (cylinder height radius-low -1))
  ([height radius-low radius-height]
   (cylinder height radius-low radius-height 0))
  ([height radius-low radius-high circular-segments]
   (cylinder height radius-low radius-high circular-segments false))
  ([height radius-low radius-height circular-segments center]
   (update-manifold  *manifold-module*
                     (fn [module]
                       (.setup module)
                       (.cylinder module height radius-low radius-height circular-segments center)))))

(defn sphere
  ([radius]
   (sphere radius 0))
  ([radius circular-segments]
   (update-manifold *manifold-module*
                    (fn [module]
                      (.setup module)
                      (.sphere module radius circular-segments)))))

(defn smooth
  ([mesh]
   (smooth mesh #js []))
  ([mesh sharpened-edges]
   (update-manifold *manifold-module*
                    (fn [module]
                      (.setup module)
                      (.smooth module mesh sharpened-edges)))))

(defn offset
  ([section delta]
   (offset section delta :square))
  ([section delta join-type]
   (offset section delta join-type 2.0))
  ([section delta join-type miter-limit]
   (offset section delta join-type miter-limit 0.0))
  ([section delta join-type miter-limit arc-tolerance]
   (update-manifold section
                    (fn [x]
                      (.offset x delta (case join-type :square 0 :round 1 :miter 2)
                               miter-limit arc-tolerance)))))

(defn extrude
  ([cross-section height]
   (extrude cross-section height 0))
  ([cross-section height n-divisions]
   (extrude cross-section height n-divisions 0.0))
  ([cross-section height n-divisions twist-degrees]
   (extrude cross-section height n-divisions twist-degrees #js [1.0 1.0]))
  ([cross-section height n-divisions twist-degrees scale-top]
   (update-manifold (js/Promise.all [*manifold-module* cross-section])
                    (fn [[module cross-section]]
                      (.setup module)
                      (.extrude module (clj->js cross-section) height n-divisions twist-degrees scale-top)))))

(defn union
  ([a] a)
  ([a b]
   (.then (js/Promise.all #js [*manifold-module* a b])
          (fn [[module & args]]
            (.setup module)
            (.union module args))))
  ([a b & more]
   (.then (js/Promise.all (list* *manifold-module* a b more))
          (fn [[module & args]]
            (.setup module)
            (.union module args)))))

(defn difference
  ([a] a)
  ([a b]
   (.then (js/Promise.all #js [*manifold-module* a b])
          (fn [[module & args]]
            (.setup module)
            (.difference module args))))
  ([a b & more]
   (.then (js/Promise.all (list* *manifold-module* a b more))
          (fn [[module & args]]
            (.setup module)
            (.difference module args)))))

(defn intersection
  ([a] a)
  ([a b]
   (.then (js/Promise.all #js [*manifold-module* a b])
          (fn [[module & args]] (.intersection module args))))
  ([a b & more]
   (.then (js/Promise.all (list* *manifold-module* a b more))
          (fn [[module & args]]
            (.setup module)
            (.intersection module args)))))

(defn hull
  ([a b]
   (.then (js/Promise.all #js [a b])
          (fn [[a b]]
            (.convexHull a b))))
  ([a b & more]
   (reduce hull (hull a b) more)))

(defn push-manifold [manifold]
  (-> manifold
      (rotate [-90, 0, 0])
      (get-mesh)
      (.then (fn [m] (createGLTF m)))))

(defn cross-section? [x]
  (p/let [mod *manifold-module*
          i x]
    (instance? mod.CrossSection i)))

(defn manifold? [x]
  (p/let [mod *manifold-module*
          i x]
    (instance? mod.Manifold i)))

(defn init [])
