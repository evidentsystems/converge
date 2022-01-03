(ns converge.io
  (:refer-clojure :exclude [read])
  (:require [clojure.java.io :as io]
            [cognitect.transit :as t]
            [converge.api :as convergent]
            [converge.domain :as domain]
            [converge.transit :as transit])
  (:import [java.security MessageDigest]
           [java.io ByteArrayOutputStream]))

(defn read-file
  [^java.io.File file]
  (when (.isFile file)
    (try
      (with-open [in (io/input-stream file)]
        (t/read (t/reader in :msgpack {:handlers transit/read-handlers})))
      (catch Exception e
        (binding [*out* *err*]
          (println (.getLocalizedMessage e)))
        nil))))

(defn ^java.io.File root-file
  [dirname]
  (io/file dirname "root.converge"))

(defn from-directory
  [dirname]
  (let [dir (io/file dirname)
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
   (pr-directory dirname :edn))
  ([dirname value-format]
   (when-let [cref (from-directory dirname)]
     (let [value @cref]
       (case value-format
         :edn
         (pr value)

         :transit-json
         (t/write (t/writer System/out :json) value)

         :transit-json-verbose
         (t/write (t/writer System/out :json-verbose) value)

         :transit-msgpack
         (t/write (t/writer System/out :msgpack) value))))))

(def file-hash-algo
  "SHA3-256")

(defn file-hash
  [^bytes file-bytes]
  (let [digest     (MessageDigest/getInstance file-hash-algo)
        hash-bytes (.digest digest file-bytes)
        sb         (StringBuilder. (* 2 (alength file-bytes)))]
    (doseq [b hash-bytes]
      (let [hex (Integer/toHexString (bit-and 0xff b))]
        (when (= (count hex) 1)
          (.append sb "0"))
        (.append sb hex)))
    (.toString sb)))

(defn sync-directory
  [source-ref dirname]
  (assert (convergent/convergent? source-ref) "First argument to sync-directory must be a ConvergentRef")
  (if-let [dest-ref (from-directory dirname)]
    (let [patch (convergent/patch-from-clock source-ref
                                             (convergent/clock dest-ref))]
      (when patch
        (let [baos (ByteArrayOutputStream.)
              _    (t/write (t/writer baos :msgpack {:handlers transit/write-handlers})
                            patch)
              file (io/file dirname (str (file-hash (.toByteArray baos))
                                         ".converge"))]
          (with-open [output-stream (io/output-stream file)]
            (.writeTo baos output-stream))
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
