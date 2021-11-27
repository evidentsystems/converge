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
(ns converge.opset.edn
  "API for interpreting an OpSet as EDN data."
  (:require [clojure.zip :as zip]
            [converge.core :as core]
            [converge.util :as util]
            [converge.opset.interpret :as interpret]))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

(defn insertion-index
  [grouped list-links entity attribute]
  (if-let [init-id (get list-links entity)]
    (loop [id init-id
           i  0]
      (if (= id attribute)
        i
        (recur (get list-links id)
               (if (contains? grouped id)
                 (inc i)
                 i))))
    []))

(defn list-insertions
  [grouped list-links entity]
  (if-let [init-id (get list-links entity)]
    (loop [id  init-id
           ins []]
      (if (= id interpret/list-end-sigil)
        ins
        (recur (get list-links id)
               (if (contains? grouped id)
                 (conj ins id)
                 ins))))
    []))

(defn interpretation-zip
  ([interpretation]
   (interpretation-zip interpretation core/root-id))
  ([interpretation root-id]
   (let [elements   (:elements interpretation)
         list-links (:list-links interpretation)
         by-entity  (group-by :entity (vals elements))
         by-attr    (group-by :attribute (vals elements))]
     (zip/zipper
      (fn [node] (-> node :children seq boolean))
      (fn [node]
        (for [{:keys [entity attribute value]}
              (:children node)]
          (let [v        (get elements value)
                a        (if (core/id? attribute)
                           (insertion-index by-attr list-links entity attribute)
                           attribute)
                children (get by-entity value)]
            {:path       (conj (:path node) a)
             :entity     value
             :attribute  attribute
             :value      v
             :insertions (list-insertions by-attr list-links value)
             :children   children})))
      (constantly nil) ;; TODO: if we need to "mutate" the zipper itself
      {:entity     root-id
       :value      (get elements root-id)
       :children   (get by-entity root-id)
       :insertions (list-insertions by-attr list-links root-id)
       :path       []}))))

(defn assoc-resizing
  ([vtr k v]
   (assoc-resizing vtr k v ::fill))
  ([vtr k v fill]
   (let [vtr-n (count vtr)
         vtr*  (if (> k vtr-n)
                 (into vtr (take (- k vtr-n) (repeat fill)))
                 vtr)]
     (assoc vtr* k v))))

(defn add-element
  [return path value]
  (let [attribute (util/last-indexed path)]
    (if attribute
      (let [parent-path (util/safe-pop path)
            parent      (util/safe-get-in return parent-path)]
        (if (sequential? parent)
          (if (seq parent-path)
            (update-in return parent-path (fnil assoc-resizing []) attribute value)
            ((fnil assoc-resizing []) return attribute value))
          (assoc-in return path value)))
      value)))

(defn assemble-values
  [interpretation]
  (loop [loc    (interpretation-zip interpretation core/root-id)
         return nil]
    (if (zip/end? loc)
      return
      (let [{:keys [path entity insertions]
             v     :value}
            (zip/node loc)]
        (recur (zip/next loc)
               (add-element return
                            path
                            (case (util/get-type v)
                              (:vec :lst)
                              (vary-meta v assoc :converge/id entity :converge/insertions insertions)

                              :map
                              (vary-meta v assoc :converge/id entity)

                              v)))))))

(defn edn
  "Transforms an converge.interpret.Interpretation into an EDN value."
  [interpretation]
  (assemble-values interpretation))
