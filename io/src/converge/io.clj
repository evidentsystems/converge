(ns converge.io
  (:refer-clojure :exclude [read])
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [taoensso.nippy :as nippy]
            converge.nippy
            [converge.api :as convergent]
            [converge.domain :as domain])
  (:import [java.security MessageDigest]))

;; TODO: validate filename hash against content?
(defn read-file
  [^java.io.File file]
  (when (.isFile file)
    (try
      (nippy/thaw-from-file file)
      (catch Exception e
        (binding [*out* *err*]
          (println (.getLocalizedMessage e)))
        nil))))

(defn ^java.io.File root-file
  [^java.io.File dir]
  (some (fn [^java.io.File f]
          (and (string/ends-with? (.getName f) ".root.converge")
               f))
        (file-seq dir)))

(defn from-directory
  [dirname]
  (let [dir (io/file dirname)
        {id       :source
         root-ops :ops}
        (some-> dir
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
  (let [[patch root?]
        (if-let [dest-ref (from-directory dirname)]
          [(convergent/patch-from-clock
            source-ref
            (convergent/clock dest-ref))]
          [(domain/->Patch
            (convergent/ref-id source-ref)
            (into {} (convergent/ref-log source-ref)))
           true])
        ba   (nippy/freeze patch)
        file (io/file dirname (str (file-hash ba) (when root? ".root") ".converge"))]
    (.mkdirs (io/file dirname))
    (with-open [output-stream (io/output-stream file)]
      (.write output-stream ba))
    (.getName file)))

(comment

  (def dirname "a-ref-dir")
  (def r (convergent/ref {:foo :bar :baz :quux}))

  (sync-directory r dirname)

  (def r (from-directory dirname))

  @r

  (swap! r assoc :quux [1 2 :lalala #{:a 'nice "set"} {:of [2 'things]}])

  (sync-directory r dirname)

  ;;
  )
