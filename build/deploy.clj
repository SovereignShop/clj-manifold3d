(ns deploy
  (:require
   [badigeon.exec :as exec]
   [badigeon.prompt :as prompt]
   [badigeon.sign :as sign]
   [badigeon.deploy :as deploy]
   [badigeon.jar :as jar]
   [clojure.string :as s]
   [clojure.tools.deps.alpha :as deps]
   [clojure.java.io :as io]))

(defn deploy-lib []
  (let [deps-data (deps/slurp-deps (io/file "deps.edn"))
        deps    (merge (:deps deps-data)
                       (-> deps-data :aliases :clj-prod :extra-deps))
        version (str (s/trim (with-out-str
                               (exec/exec "git" {:proc-args ["describe" "--tags"]
                                                 ;; The error message of the exception thrown upon error.
                                                 :error-msg "Failed to get tags"})))
                     ".4-SNAPSHOT")]
    (assert (re-find #"^\d.\d.\d.4-SNAPSHOT$" version))
    (jar/jar 'org.clojars.cartesiantheatrics/clj-manifold3d {:mvn/version version}
             {:out-path                (format "target/clj-manifold3d-%s.jar" version)
              :paths                   ["src/clj" "src/cljc"]
              :deps                    deps
              :mvn/repos               '{"clojars" {:url "https://repo.clojars.org/"}}
              :exclusion-predicate     jar/default-exclusion-predicate
              :allow-all-dependencies? true})
    (let [artifacts (-> [{:file-path (format "target/clj-manifold3d-%s.jar" version)
                          :extension "jar"}
                         {:file-path "pom.xml"
                          :extension "pom"}]
                        (badigeon.sign/sign {:command "gpg"
                                             :gpg-key "120A7E7A7D1722C5"}))]
      (deploy/deploy 'org.clojars.cartesiantheatrics/clj-manifold3d
                     version
                     artifacts
                     {:url "https://repo.clojars.org/"
                      :id "clojars"}))))

(defn -main [& args]
  (deploy-lib) )
