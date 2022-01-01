(ns converge.io
  (:refer-clojure :exclude [read])
  (:require [clojure.java.io :as io]
            [cognitect.transit :as t]
            [converge.api :as convergent]
            [converge.domain :as domain]
            [converge.transit :as transit]))

(def formats
  #{:edn
    :transit-json
    :transit-json-verbose
    :transit-msgpack})

(def default-value-format
  :edn)

(defn read-file
  [^java.io.File file]
  (when (.isFile file)
    (try
      (with-open [input-stream (io/input-stream file)]
        (t/read (t/reader input-stream :msgpack {:handlers transit/read-handlers})))
      (catch Exception _ nil))))

(defn ^java.io.File root-file
  [dirname]
  (io/file dirname "root.converge"))

(defn from-directory
  [dirname]
  (let [dir  (io/file dirname)
        {id       :source
         root-ops :ops}
        (some-> dirname
                root-file
                read-file)]
    (when (and (.isDirectory dir)
               (uuid? id)
               (seq root-ops))
      (convergent/ref-from-ops
       (reduce (fn [agg file]
                 (if (.isFile file)
                   (let [{:keys [source ops]}
                         (read-file file)]
                     (if (= source id)
                       (into agg ops)
                       agg))
                   agg))
               root-ops
               (file-seq dir))))))

(defn pr-directory
  ([dirname]
   (pr-directory dirname default-value-format))
  ([dirname value-format]
   (let [value @(from-directory dirname)]
     (case value-format
       :edn
       (pr value)

       :transit-json
       (t/write (t/writer System/out :json) value)

       :transit-json-verbose
       (t/write (t/writer System/out :json-verbose) value)

       :transit-msgpack
       (t/write (t/writer System/out :msgpack) value)))))

(defn sync-directory
  [source-ref dirname]
  (assert (convergent/convergent? source-ref) "First argument to sync-directory must be a ConvergentRef")
  (if-let [dest-ref (from-directory dirname)]
    (let [patch (convergent/patch-from-clock source-ref
                                             (convergent/clock dest-ref))]
      (when patch
        (let [file (io/file dirname (str (hash patch) ".converge"))]
          (with-open [output-stream (io/output-stream file)]
            (t/write (t/writer output-stream :msgpack {:handlers transit/write-handlers})
                     patch))
          (.getName file))))
    (let [file (root-file dirname)]
      (.mkdirs (io/file dirname))
      (with-open [output-stream (io/output-stream file)]
        (t/write (t/writer output-stream :msgpack {:handlers transit/write-handlers})
                 (domain/->Patch
                  (convergent/ref-id source-ref)
                  (convergent/ref-log source-ref))))
      (.getName file))))

(comment

  (def dirname "a-ref-dir")
  (def r (convergent/ref {:foo :bar :baz :quux}))

  (sync-directory r dirname)

  (def r (from-directory dirname))

  (pr-directory dirname)

  @r

  (swap! r assoc :quux [1 2 :lalala #{:a 'nice "set"} {:of [2 'things]}])

  (sync-directory r dirname)

  ;;
  )
