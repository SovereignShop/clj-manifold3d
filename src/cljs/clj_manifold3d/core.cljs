(ns clj-manifold3d.core
  (:require ["/clj_manifold3d/manifold" :as manifold3d]
            [clj-manifold3d.vars :as vars]))

(def manifold-module ((.-default manifold3d)))

(defn manifold
  ([]
   (manifold #js {}))
  ([mesh]
   (.then manifold-module
          (fn [mod]
            (.setup mod)
            (mod.Manifold. (mod.Mesh. (clj->js mesh)))))))

(defn warp
  [manifold func]
  (.then manifold (fn [man]
                    (.warp man func))))

(defn translate
  [manifold & vec]
  (.then manifold (fn [man] (.translate man (clj->js vec)))))

(defn rotate
  [manifold vec]
  (.then manifold (fn [man] (.rotate man (clj->js vec)))))

(defn scale
  [manifold vec]
  (.then manifold (fn [man] (.scale man (clj->js vec)))))

(defn mirror
  [manifold vec]
  (.then manifold (fn [man] (.mirror man (clj->js vec)))))

(defn trim-by-plane
  [manifold normal offset]
  (.then manifold (fn [man] (.trimByPlane man (clj->js normal) offset))))

(defn get-mesh
  [manifold]
  (.then manifold (fn [man] (.getMesh man))))

(defn decompose
  [manifold]
  (.then manifold (fn [man] (.decompose man))))

(defn get-curvature
  [manifold]
  (.then manifold (fn [man] (.getCurvature man))))

(defn cube
  ([xyz center?]
   (.then manifold-module
          (fn [module]
            (.setup module)
            (.cube module (clj->js xyz) center?))))
  ([x y z]
   (.then manifold-module
          (fn [module]
            (.setup module)
            (.cube module #js [x y z])))))

(defn cylinder
  ([height radius-low]
   (cylinder height radius-low -1))
  ([height radius-low radius-height]
   (cylinder height radius-low radius-height 0))
  ([height radius-low radius-high circular-segments]
   (cylinder height radius-low radius-high circular-segments false))
  ([height radius-low radius-height circular-segments center]
   (.then manifold-module
          (fn [module]
            (.setup module)
            (.cylinder module height radius-low radius-height circular-segments center)))))

(defn sphere
  ([radius]
   (sphere radius 0))
  ([radius circular-segments]
   (.then manifold-module
          (fn [module]
            (.setup module)
            (.sphere module radius circular-segments)))))

(defn smooth
  ([mesh]
   (smooth mesh #js []))
  ([mesh sharpened-edges]
   (.then manifold-module
          (fn [module]
            (.setup module)
            (.smooth module mesh sharpened-edges)))))

(defn extrude
  ([polygons height]
   (extrude polygons height 0))
  ([polygons height n-divisions]
   (extrude polygons height n-divisions 0.0))
  ([polygons height n-divisions twist-degrees]
   (extrude polygons height n-divisions twist-degrees #js [1.0 1.0]))
  ([polygons height n-divisions twist-degrees scale-top]
   (.then manifold-module
          (fn [module]
            (.setup module)
            (.extrude module polygons height n-divisions twist-degrees scale-top)))))

(defn triangulate
  ([polygons]
   (triangulate polygons -1))
  ([polygons precision]
   (.then manifold-module (fn [module] (.triangulate module polygons precision)))))

(defn revolve
  ([polygons]
   (revolve polygons 0))
  ([polygons circular-segments]
   (.then manifold-module (fn [module] (.revolve module polygons circular-segments)))))

(defn compose
  ([manifolds]
   (.then manifold-module (fn [module] (.compose module manifolds)))))

#_(defn level-set
  ([manifold sdf bounds edge-length]
   (level-set manifold sdf bounds edge-length 0))
  ([manifold sdf bounds edge-length level]
   (.then manifold (fn [man] (.levelSet man sdf bounds edge-length level)))))

(defn union
  ([a] a)
  ([a b]
   (.then (js/Promise.all #js [manifold-module a b])
          (fn [[module & args]]
            (.setup module)
            (.union module args))))
  ([a b & more]
   (.then (js/Promise.all (list* manifold-module a b more))
          (fn [[module & args]]
            (.setup module)
            (.union module args)))))

(defn difference
  ([a] a)
  ([a b]
   (.then (js/Promise.all #js [manifold-module a b])
          (fn [[module & args]]
            (.setup module)
            (.difference module args))))
  ([a b & more]
   (.then (js/Promise.all (list* manifold-module a b more))
          (fn [[module & args]]
            (.setup module)
            (.difference module args)))))

(defn intersection
  ([a] a)
  ([a b]
   (.then (js/Promise.all #js [manifold-module a b])
          (fn [[module & args]] (.intersection module args))))
  ([a b & more]
   (.then (js/Promise.all (list* manifold-module a b more))
          (fn [[module & args]]
            (.setup module)
            (.intersection module args)))))

(comment

  (def a (cylinder 4 10))
  (def b (sphere 10))
  (def c (cube [3 4 5] true))

  (-> (extrude (clj->js [[5 0] [0 5] [-5 0]])
               200)
      (get-mesh)
      (.then (fn [x] (js/console.log x))))

  (-> (union a b c)
      (translate 4 5 3)
      (get-mesh)
      (.then (fn [mesh x] (js/console.log mesh)) ))

  )


(defn init [])
