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
(ns converge.opset.interpret
  "Functions for interpreting an OpSet as per sections 3.2 and 5.2 of
  the [OpSets paper](https://arxiv.org/pdf/1805.04263.pdf)"
  (:require [clojure.data.avl :as avl]
            [converge.opset.ops :as ops]
            [converge.domain :as domain])
  (:import [clojure.data.avl AVLSet]
           #?(:clj [converge.domain Id])))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

(defrecord Element [#?@(:cljs [^clj entity]
                        :clj  [^Id  entity])
                    #?@(:cljs [^clj attribute]
                        :clj  [^Id  attribute])
                    #?@(:cljs [^clj value]
                        :clj  [^Id  value])
                    #?@(:cljs [^clj id]
                        :clj  [^Id  id])]
  #?(:clj  Comparable
     :cljs IComparable)
  (#?(:clj  compareTo
      :cljs -compare)
    [_ other]
    (assert (instance? Element other))
    (compare
     [entity          attribute          value          id]
     [(:entity other) (:attribute other) (:value other) (:id other)])))

(defn entity-start-element
  [entity]
  (->Element entity nil nil nil))

(defn entity-end-element
  [entity]
  (->Element entity domain/highest-id nil nil))

(defn entity-attribute-start-element
  [entity attribute]
  (->Element entity attribute nil nil))

(defn entity-attribute-end-element
  [entity attribute]
  (->Element entity attribute domain/highest-id nil))

(defrecord Interpretation [elements list-links
                           parents  entities
                           keys     key-cache
                           values])

(defn make-interpretation
  [{:keys [elements list-links entities keys values]}]
  (assert (coll? elements))
  (assert (map? list-links))
  (assert (map? entities))
  (assert (map? keys))
  (assert (map? values))
  (let [elements* (if (instance? AVLSet elements)
                    elements
                    (into (avl/sorted-set) elements))]
    (->Interpretation
     elements*
     list-links
     (persistent!
      (reduce (fn [agg element]
                (assoc! agg (:value element) (:entity element)))
              (transient {})
              elements*))
     entities
     keys
     (persistent!
      (reduce-kv (fn [agg k v]
                   (assoc! agg (:value v) k))
                 (transient {})
                 keys))
     values)))

(def list-end-sigil ::list-end)

(defmulti -interpret-op
  (fn [_agg _id op] (:action op))
  :default ::default)

(defmethod -interpret-op ::default
  [agg _id _op]
  agg)

(defmethod -interpret-op ops/MAKE_MAP
  [agg id op]
  (update agg :entities assoc! id (assoc (:data op) :value {})))

(defmethod -interpret-op ops/MAKE_VECTOR
  [agg id op]
  (-> agg
      (update :list-links assoc! id list-end-sigil)
      (update :entities assoc! id (assoc (:data op) :value []))))

(defmethod -interpret-op ops/MAKE_SET
  [agg id op]
  (update agg :entities assoc! id (assoc (:data op) :value #{})))

(defmethod -interpret-op ops/MAKE_LIST
  [agg id op]
  (-> agg
      (update :list-links assoc! id list-end-sigil)
      (update :entities assoc! id (assoc (:data op) :value ()))))

(defmethod -interpret-op ops/MAKE_KEY
  [agg id op]
  (-> agg
      (update :keys assoc! id (:data op))
      (update :key-cache assoc! (-> op :data :value) id)))

(defmethod -interpret-op ops/MAKE_VALUE
  [agg id op]
  (update agg :values assoc! id (:data op)))

(defmethod -interpret-op ops/INSERT
  [{:keys [list-links] :as agg} id {{prev :after} :data}]
  (let [next (get list-links prev)]
    (if next
      (assoc agg
             :list-links
             (-> list-links
                 (dissoc! prev)
                 (assoc! prev id
                         id next)))
      agg)))

(defn is-ancestor?
  [parents entity possible-ancestor]
  (loop [self entity]
    (if-some [parent (get parents self)]
      (if (= parent possible-ancestor)
        true
        (recur parent))
      false)))

;; We use the adjustment for single-value register defined in 3.2
(defmethod -interpret-op ops/ASSIGN
  [{:keys [elements parents entities] :as agg} id {:keys [data]}]
  (let [{:keys [entity attribute value]} data

        elements* (persistent! elements)
        parent    (get parents value)]
    (if (is-ancestor? parents entity value)
      agg
      (reduce (fn [agg element]
                (if (= (:value element) value)
                  (update agg :elements disj! element)
                  agg))
              (reduce (fn [agg element]
                        (update agg :elements disj! element))
                      (assoc agg
                             :elements (conj!
                                        (transient elements*)
                                        (->Element entity attribute value id))
                             :parents (assoc! parents value entity))
                      (avl/subrange elements*
                                    >= (entity-attribute-start-element entity attribute)
                                    <  (entity-attribute-end-element   entity attribute)))
              (when (contains? entities value)
                (avl/subrange elements*
                              >= (entity-start-element parent)
                              <  (entity-end-element   parent)))))))

;; We use the adjustment for single-value register defined in 3.2
(defmethod -interpret-op ops/REMOVE
  [{:keys [elements] :as agg} _id {:keys [data]}]
  (let [{:keys [entity attribute]} data

        elements* (persistent! elements)]
    (reduce (fn [agg element]
              (-> agg
                  (update :elements disj! element)
                  (update :parents dissoc! (:value element))))
            (assoc agg :elements (transient elements*))
            (avl/subrange elements*
                          >= (entity-attribute-start-element entity attribute)
                          <  (entity-attribute-end-element   entity attribute)))))

(defn interpret
  ([opset-log]
   (interpret nil opset-log))
  ([{:keys [elements list-links
            parents  entities
            keys     key-cache
            values]
     :as   _interpretation}
    ops]
   (let [{elements*   :elements
          list-links* :list-links
          parents*    :parents
          entities*   :entities
          keys*       :keys
          key-cache*  :key-cache
          values*     :values}
         (reduce-kv -interpret-op
                    {:elements   (transient (or elements (avl/sorted-set)))
                     :list-links (transient (or list-links {}))
                     :parents    (transient (or parents {}))
                     :entities   (transient (or entities {}))
                     :keys       (transient (or keys {}))
                     :key-cache  (transient (or key-cache {}))
                     :values     (transient (or values {}))}
                    ops)]
     (map->Interpretation
      {:elements   (persistent! elements*)
       :list-links (persistent! list-links*)
       :parents    (persistent! parents*)
       :entities   (persistent! entities*)
       :keys       (persistent! keys*)
       :key-cache  (persistent! key-cache*)
       :values     (persistent! values*)}))))
