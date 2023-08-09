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
  "API for converting an OpSet interpretation as EDN data."
  (:require [clojure.data.avl :as avl]
            [converge.domain :as domain]
            [converge.opset.interpret :as interpret]))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

(defmulti -edn
  (fn [{:keys [entities values]} entity]
    (if-some [e (get-in entities [entity :value])]
      (domain/get-type e)
      (domain/get-type (get-in values [entity :value]))))
  :default ::default)

(defmethod -edn ::default
  [{:keys [values keys]} entity]
  (if-some [value (get-in values [entity :value])]
    value
    (get-in keys [entity :value])))

(defmethod -edn :map
  [{:keys [elements] :as interpretation} entity]
  (let [attrs (avl/subrange elements
                            >= (interpret/entity-start-element entity)
                            <  (interpret/entity-end-element   entity))]
    (loop [i   0
           ret (transient {})]
      (if-let [element (nth attrs i nil)]
        (let [k (-edn interpretation (:attribute element))]
          (recur (inc i)
                 (assoc! ret
                         k
                         (-edn interpretation (:value element)))))
        (some-> ret
                persistent!
                (vary-meta assoc :converge/id entity))))))

(defn- -edn-vector
  [{:keys [elements list-links] :as interpretation}
   entity]
  (let [attrs (persistent!
               (reduce (fn [agg {:keys [attribute value]}]
                         (assoc! agg attribute value))
                       (transient {})
                       (avl/subrange elements
                                     >= (interpret/entity-start-element entity)
                                     <  (interpret/entity-end-element   entity))))]
    (loop [ins (get list-links entity)
           ret (transient [])
           idx (transient [])]
      (if (= ins interpret/list-end-sigil)
        (some-> ret
                persistent!
                (vary-meta assoc
                           :converge/id entity
                           :converge/insertions (persistent! idx)))
        (let [[next-ret next-idx]
              (if-some [value (get attrs ins)]
                [(conj! ret (-edn interpretation value))
                 (conj! idx ins)]
                [ret idx])]
          (recur (get list-links ins)
                 next-ret
                 next-idx))))))

(defmethod -edn :vec
  [interpretation entity]
  (-edn-vector interpretation entity))

(defmethod -edn :set
  [{:keys [elements] :as interpretation} entity]
  (let [attrs (avl/subrange elements
                            >= (interpret/entity-start-element entity)
                            <  (interpret/entity-end-element   entity))]
    (loop [i   0
           ret (transient #{})]
      (if-let [element (nth attrs i nil)]
        (let [member (-edn interpretation (:attribute element))]
          (recur (inc i)
                 (conj! ret member)))
        (some-> ret
                persistent!
                (vary-meta assoc :converge/id entity))))))

(defmethod -edn :lst
  [interpretation entity]
  (let [v (-edn-vector interpretation entity)]
    (with-meta (apply list v) (meta v))))

(defn root-element-id
  [{:keys [values entities]}]
  (reduce-kv (fn [agg id {:keys [root?]}]
               (if root?
                 (if (nat-int? (compare id agg)) id agg)
                 agg))
             domain/root-id
             (into entities values)))

(defn edn
  "Transforms an converge.opset.interpret.Interpretation into an EDN value."
  [interpretation]
  (-edn interpretation (root-element-id interpretation)))
