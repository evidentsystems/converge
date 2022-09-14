(ns build
  (:refer-clojure :exclude [test compile])
  (:require [clojure.tools.build.api :as b]
            [org.corfield.build :as bb]))

(def lib 'converge/converge)
(def version (format "0.1.%s" (b/git-count-revs nil)))

(defn test "Run the tests." [opts]
  (bb/run-tests opts))

(defn ci "Run the CI pipeline of tests (and build the JAR)." [opts]
  (-> opts
      (assoc :lib lib :version version)
      (bb/run-tests)
      (bb/clean)
      (bb/jar)))

(defn compile "Compile Clojure" [opts]
  (let [opts* (merge
               opts
               {:basis     (bb/default-basis (:basis opts))
                :src-dirs  (or (:basis opts) ["src"])
                :class-dir (bb/default-class-dir (:class-dir opts))})]
    (b/compile-clj opts*)
    opts*))

(defn install "Install the JAR locally." [opts]
  (-> opts
      (assoc :lib lib :version version)
      (bb/install)))

(defn deploy "Deploy the JAR to Clojars." [opts]
  (-> opts
      (assoc :lib lib :version version)
      (bb/deploy)))
