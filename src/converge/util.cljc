(ns converge.util
  "Utility functions"
  #?(:cljs (:refer-clojure :exclude [uuid]))
  (:require #?@(:clj
                [[clj-uuid :as uuid]]
                :cljs
                [[uuid :as uuid]])))

(set! *warn-on-reflection* true)

(defn uuid
  []
  (uuid/v4))

(defn now
  []
  (inst-ms
   #?(:clj  (java.util.Date.)
      :cljs (js/Date.))))

(defn safe-get
  ([coll i]
   (safe-get coll i nil))
  ([coll i not-found]
   (if (map? coll)
     (get coll i not-found)
     (try
       (or (nth coll i) not-found)
       (catch Exception e not-found)))))

(defn safe-get-in
  ([m ks]
   (safe-get-in m ks nil))
  ([m ks not-found]
   (loop [sentinel (Object.)
          m        m
          ks       (seq ks)]
     (if ks
       (let [m (safe-get m (first ks) sentinel)]
         (if (identical? sentinel m)
           not-found
           (recur sentinel m (next ks))))
       m))))

(defn get-id
  [o]
  (some-> o meta :converge/id))

(defn get-insertion-id
  [o n]
  (some-> o meta :converge/insertions (safe-get n)))

(defn get-id-in
  [m ks]
  (some-> m
          (safe-get-in ks)
          get-id))

#_(defn slice-by
    [sorted k v]
    (let []))
