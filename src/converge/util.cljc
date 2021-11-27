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
            [editscript.edit :as edit]
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

(def get-type edit/get-type)

;; HT: https://github.com/juji-io/editscript/blob/ddb13130d16ae920d1ead8ae77b23c24a202e92e/src/editscript/patch.cljc#L18
(defn safe-get
  ([x p]
   (safe-get x p nil))
  ([x p not-found]
   (case (get-type x)
     (:map :vec :set) (get x p not-found)
     :lst             (nth x p not-found))))

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

(defn first-indexed
  [indexed]
  (nth indexed 0 nil))

(defn last-indexed
  [indexed]
  (nth indexed (dec (count indexed)) nil))

(defn safe-pop
  [indexed]
  (when-not (empty? indexed)
    (pop indexed)))

(defn queue
  [& elems]
  (into #?(:clj  clojure.lang.PersistentQueue/EMPTY
           :cljs (.-EMPTY PersistentQueue))
        elems))

#_(defn slice-by
    [sorted k v]
    (let []))
