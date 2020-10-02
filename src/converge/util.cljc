(ns converge.util
  "Utility functions"
  #?(:cljs (:refer-clojure :exclude [uuid]))
  (:require #?@(:clj
                [[clj-uuid :as uuid]]
                :cljs
                [[uuid :as uuid]])))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

(defn uuid
  []
  #?(:clj  (uuid/v4)
     :cljs (random-uuid)))

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
       (catch #?(:clj Exception
                 :cljs :default) e not-found)))))

(def lookup-sentinel
  #?(:clj  (Object.)
     :cljs (js-obj)))

(defn safe-get-in
  ([m ks]
   (safe-get-in m ks nil))
  ([m ks not-found]
   (loop [sentinel lookup-sentinel
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

(defn queue
  [& elems]
  (into #?(:clj  clojure.lang.PersistentQueue/EMPTY
           :cljs (.-EMPTY PersistentQueue))
        elems))

#_(defn slice-by
    [sorted k v]
    (let []))
