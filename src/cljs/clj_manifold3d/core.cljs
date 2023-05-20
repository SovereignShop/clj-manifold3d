(ns clj-manifold3d.core
  (:require
   [promesa.core :as p]
   ["/clj_manifold3d/manifold" :as manifold3d]
   ["/clj_manifold3d/model_viewer" :as mv :refer [createGLTF]]))

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

(defn warp
  [manifold func]
  (update-manifold manifold (fn [man] (.warp man func))))

(defn translate
  [manifold & vec]
  (update-manifold manifold (fn [man] (.translate man (clj->js vec)))))

(defn rotate
  [manifold vec]
  (update-manifold manifold (fn [man] (.rotate man (clj->js vec)))))

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
                      (.cube module #js [x y z])))))

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
   (offset))
  ([section delta join-type])
  ([section delta join-type miter-limit])
  ([section dleta join-type miter-limit arc-tolerance]
   ))

(defn extrude
  ([polygons height]
   (extrude polygons height 0))
  ([polygons height n-divisions]
   (extrude polygons height n-divisions 0.0))
  ([polygons height n-divisions twist-degrees]
   (extrude polygons height n-divisions twist-degrees #js [1.0 1.0]))
  ([polygons height n-divisions twist-degrees scale-top]
   (update-manifold *manifold-module*
                    (fn [module]
                      (.setup module)
                      (.extrude module polygons height n-divisions twist-degrees scale-top)))))

(defn triangulate
  ([polygons]
   (triangulate polygons -1))
  ([polygons precision]
   (update-manifold *manifold-module*
                    (fn [module]
                      (.setup module)
                      (.triangulate module polygons precision)))))

(defn revolve
  ([polygons]
   (revolve polygons 0))
  ([polygons circular-segments]
   (update-manifold *manifold-module*
                    (fn [module]
                      (.setup module)
                      (.revolve module polygons circular-segments)))))

(defn compose
  ([manifolds]
   (update-manifold *manifold-module*
                    (fn [module]
                      (.setup module)
                      (.compose module manifolds)))))

(defn level-set
  ([manifold sdf bounds edge-length]
   (level-set manifold sdf bounds edge-length 0))
  ([manifold sdf bounds edge-length level]
   (update-manifold manifold (fn [man] (.levelSet man sdf bounds edge-length level)))))

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

(comment

  (println "hello")

  (push-manifold
   (p/let [y  20
           man (cube 20 20 y)]
     (-> man
         (translate 3 3 10)
         (rotate [-90 0 0]))))

  (go
    (let [man (<! (promise-chan (cube 10 10 40)))]
      (-> man
          (translate [0 0 10])
          (rotate [-90 0 0]))))


  (push-manifold (cube 10 10 40))

  (push-manifold
   (difference
    (cube [10 10 30] true)
    (-> (cube [4 4 100] true)
        (translate 0 0 -4))))

  (push-manifold
   (difference
    (hull (-> (cylinder 2 20 20 100))
          (-> (sphere 5 100)
              (translate 0 0 30)))
    #_(-> (cylinder 100 8 8 100)
        (translate 0 0 -1))))

  (js/console.log "wtf")

  (.then
   (get-mesh
    (hull (-> (cube 15 15 2)
              (translate 10 10 0))
          (-> (cube  20 2)
              (translate 10 10 70))))
   (fn [mesh]
     (js/console.log mesh)))


  (push-manifold (cube 5 5 10) )

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
