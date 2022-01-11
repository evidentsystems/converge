(ns converge.bench
  (:require [clojure.edn :as edn]
            #?@(:clj  [[clojure.java.io :as io]
                       [criterium.core :as criterium]]
                :cljs [["fs" :as fs]])
            [converge.api :as convergent]
            [converge.opset.interpret :as interpret]
            [converge.opset.edn :as ednize])
  #?(:clj (:gen-class)))

(defn read-edn-file
  [path]
  #?(:clj
     (with-open [pbr (java.io.PushbackReader. (io/reader (io/file path)))]
       (edn/read pbr))

     :cljs
     (edn/read-string (.readFileSync fs path "utf8"))))

(defn -main
  [& args]
  (let [value (read-edn-file (first args))]
    (println "Ref Creation...")
    #?(:clj
       (criterium/bench (convergent/ref value))

       :cljs
       (simple-benchmark
        []
        (convergent/ref value)
        1000))

    (let [r     (convergent/ref value)
          log   (convergent/ref-log r)]
      (println "Interpretation...")
      #?(:clj
         (criterium/bench (interpret/interpret log))

         :cljs
         (simple-benchmark
          []
          (interpret/interpret log)
          1000)))

    (let [r     (convergent/ref value)
          log   (convergent/ref-log r)
          i     (interpret/interpret log)]
      (println "Edn-izing the interpretation...")
      #?(:clj
         (criterium/bench (ednize/edn i))

         :cljs
         (simple-benchmark
          []
          (ednize/edn i)
          1000)))))
