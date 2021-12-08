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
            [converge.core :as core])
  #?(:clj (:import [converge.core Id])))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

(declare element?)

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
    (assert (element? other))
    (compare
     [entity          attribute          value          id]
     [(:entity other) (:attribute other) (:value other) (:id other)])))

(defn element?
  [o]
  (instance? Element o))

(defrecord Interpretation [elements list-links values])

(def list-end-sigil ::list-end)

(defmulti -interpret-op
  (fn [_agg _id op] (:action op))
  :default ::default)

(defmethod -interpret-op ::default
  [agg _id _op]
  agg)

(defmethod -interpret-op ops/MAKE_MAP
  [agg id op]
  (update agg :values assoc! id (assoc (:data op) :value {})))

(defmethod -interpret-op ops/MAKE_VECTOR
  [agg id op]
  (-> agg
      (update :list-links assoc! id list-end-sigil)
      (update :values assoc! id (assoc (:data op) :value []))))

(defmethod -interpret-op ops/MAKE_SET
  [agg id op]
  (update agg :values assoc! id (assoc (:data op) :value #{})))

(defmethod -interpret-op ops/MAKE_LIST
  [agg id op]
  (-> agg
      (update :list-links assoc! id list-end-sigil)
      (update :values assoc! id (assoc (:data op) :value ()))))

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

;; We use the broader interpretation algorithm defined in section 3.2,
;; rather than the narrower tree definition defined in 5.2, since
;; we're generating Ops from trees. We use the adjustment for single-value register.
(defmethod -interpret-op ops/ASSIGN
  [{:keys [elements] :as agg} id {:keys [data]}]
  (let [{:keys [entity attribute value]} data

        elements* (persistent! elements)]
    (reduce (fn [agg element]
              (update agg :elements disj! (avl/nearest elements* >= element)))
            (assoc agg :elements (conj!
                                  (transient elements*)
                                  (->Element entity attribute value id)))
            (avl/subrange elements*
                          >= (->Element entity attribute nil nil)
                          <  (->Element entity (core/successor-id attribute) nil nil)))))

;; We use the broader interpretation algorithm defined in section 3.2,
;; rather than the narrower tree definition defined in 5.2, since
;; we're generating Ops from trees. We use the adjustment for single-value register.
(defmethod -interpret-op ops/REMOVE
  [{:keys [elements] :as agg} _id {:keys [data]}]
  (let [{:keys [entity attribute]} data

        elements* (persistent! elements)]
    (reduce (fn [agg element]
              (update agg :elements disj! (avl/nearest elements* <= element)))
            (assoc agg :elements (transient elements*))
            (avl/subrange elements*
                          >= (->Element entity attribute nil nil)
                          <  (->Element entity (core/successor-id attribute) nil nil)))))

(defn interpret
  ([opset-log]
   (interpret nil opset-log))
  ([{:keys [elements list-links values] :as _interpretation} ops]
   (let [elements-init   (or elements   (avl/sorted-set))
         list-links-init (or list-links {})
         values          (or values     {})

         {elements*   :elements
          list-links* :list-links
          values*     :values}
         (reduce-kv -interpret-op
                    {:elements   (transient elements-init)
                     :list-links (transient list-links-init)
                     :values     (transient values)}
                    ops)]
     (map->Interpretation
      {:elements   (persistent! elements*)
       :list-links (persistent! list-links*)
       :values     (persistent! values*)}))))
