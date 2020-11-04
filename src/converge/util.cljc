;; Copyright 2020 Evident Systems LLC

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
(ns converge.util
  "Utility functions"
  #?(:cljs (:refer-clojure :exclude [uuid]))
  (:require [clojure.string :as string]
            #?@(:clj
                [[clj-uuid :as uuid]]
                :cljs
                [[uuid :as uuid]])))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

(defn uuid
  ([]
   #?(:clj  (uuid/v4)
      :cljs (random-uuid)))
  ([uuid-str]
   (cond
     (uuid? uuid-str)
     uuid-str

     (string/blank? uuid-str)
     nil

     (string? uuid-str)
     #?(:clj
        (java.util.UUID/fromString uuid-str)
        :cljs
        (if (uuid/validate uuid-str)
          (cljs.core/uuid uuid-str)
          (throw (ex-info "invalid UUID string" {:uuid-str uuid-str}))))

     :else
     (throw (ex-info "cannot make UUID from object" {:object uuid-str})))))

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

(defn queue
  [& elems]
  (into #?(:clj  clojure.lang.PersistentQueue/EMPTY
           :cljs (.-EMPTY PersistentQueue))
        elems))

#_(defn slice-by
    [sorted k v]
    (let []))
