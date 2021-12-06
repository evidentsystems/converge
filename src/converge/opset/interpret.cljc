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
            [converge.core :as core]))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

(declare element?)

(defrecord Element [#?@(:cljs [^clj entity]
                        :clj  [entity])
                    attribute
                    #?@(:cljs [^clj value]
                        :clj  [value])]
  #?(:clj  Comparable
     :cljs IComparable)
  (#?(:clj  compareTo
      :cljs -compare)
    [_ other]
    (assert (element? other))
    (compare
     [entity          attribute          value]
     [(:entity other) (:attribute other) (:value other)])))

(defn element?
  [o]
  (instance? Element o))

(defrecord Interpretation [elements list-links])

(defn elements->eavt
  [elements]
  (persistent!
   (reduce-kv (fn [agg k v]
                (if (element? v)
                  (conj! agg (vary-meta v assoc :converge/id k))
                  agg))
              (transient (avl/sorted-set))
              elements)))

(def list-end-sigil ::list-end)

(defmulti -interpret-op
  (fn [_agg _id op] (:action op))
  :default ::default)

(defmethod -interpret-op ::default
  [agg _id _op]
  agg)

(defmethod -interpret-op ops/MAKE_MAP
  [agg id op]
  (update agg :elements assoc! id (assoc (:data op) :value {})))

(defmethod -interpret-op ops/MAKE_VECTOR
  [agg id op]
  (-> agg
      (update :list-links assoc! id list-end-sigil)
      (update :elements assoc! id (assoc (:data op) :value []))))

(defmethod -interpret-op ops/MAKE_SET
  [agg id op]
  (update agg :elements assoc! id (assoc (:data op) :value #{})))

(defmethod -interpret-op ops/MAKE_LIST
  [agg id op]
  (-> agg
      (update :list-links assoc! id list-end-sigil)
      (update :elements assoc! id (assoc (:data op) :value ()))))

(defmethod -interpret-op ops/MAKE_VALUE
  [agg id op]
  (update agg :elements assoc! id (:data op)))

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

;; We use the broader interpretation algorithm defined in section 3.2,
;; rather than the narrower tree definition defined in 5.2, since
;; we're generating Ops from trees. We use the adjustment for single-value register.
(defmethod -interpret-op ops/ASSIGN
  [{:keys [eavt] :as agg} id {:keys [data]}]
  (let [{:keys [entity attribute value]} data

        eavt* (persistent! eavt)]
    (reduce (fn [agg element]
              (if-let [id (get (meta element) :converge/id)]
                (-> agg
                    (update :elements dissoc! id)
                    (update :eavt disj! element))
                agg))
            (let [element (->Element entity attribute value)]
              (-> agg
                  (assoc :eavt (-> eavt*
                                   transient
                                   (conj! (vary-meta element assoc :converge/id id))))
                  (update :elements assoc! id element)))
            (avl/subrange eavt*
                          >= (->Element entity attribute nil)
                          <  (->Element entity (core/successor-id attribute) nil)))))

;; We use the broader interpretation algorithm defined in section 3.2,
;; rather than the narrower tree definition defined in 5.2, since
;; we're generating Ops from trees. We use the adjustment for single-value register.
(defmethod -interpret-op ops/REMOVE
  [{:keys [eavt] :as agg} _id {:keys [data]}]
  (let [{:keys [entity attribute]} data

        eavt* (persistent! eavt)]
    (reduce (fn [agg element]
              (if-let [id (get (meta element) :converge/id)]
                (-> agg
                    (update :elements dissoc! id)
                    (update :eavt disj! element))
                agg))
            (assoc agg :eavt (transient eavt*))
            (avl/subrange eavt*
                          >= (->Element entity attribute nil)
                          <  (->Element entity (core/successor-id attribute) nil)))))

(defmethod -interpret-op ops/SNAPSHOT
  [agg id
   {{:keys [log-hash]
     {:keys [elements list-links]}
     :interpretation}
    :data}]
  (if (= log-hash (hash (avl/subrange (:opset agg) < id)))
    (assoc agg
           :eavt       (transient (elements->eavt elements))
           :elements   (transient elements)
           :list-links (transient list-links))
    agg))

(defn interpret
  ([opset-log]
   (interpret nil opset-log))
  ([{:keys [elements list-links eavt] :as _interpretation} ops]
   (let [elements-init   (or elements {})
         list-links-init (or list-links {})
         eavt-init       (or eavt (elements->eavt elements-init))

         {elements*   :elements
          list-links* :list-links
          eavt*       :eavt}
         (reduce-kv -interpret-op
                    {:elements   (transient elements-init)
                     :list-links (transient list-links-init)
                     :eavt       (transient eavt-init)
                     :opset      ops}
                    ops)]
     (map->Interpretation
      {:elements   (persistent! elements*)
       :list-links (persistent! list-links*)
       :eavt       (persistent! eavt*)}))))
