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
            [converge.core :as core]
            [converge.util :as util]
            [converge.opset.interpret :as interpret]))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

(defn- elements->eavt
  [elements]
  (persistent!
   (reduce-kv (fn [agg _k v]
                (if (interpret/element? v)
                  (conj! agg v)
                  agg))
              (transient (avl/sorted-set))
              elements)))

(defmulti -edn
  (fn [{:keys [elements entity]}]
    (some-> elements
            (get-in [entity :value])
            util/get-type))
  :default ::default)

(defmethod -edn ::default
  [{:keys [elements entity]}]
  (get-in elements [entity :value]))

;; TODO: build metadata index of (hash element) -> value id
;; TODO: lookup element value by attribute (which is a value id, not a static value)
(defmethod -edn :map
  [{:keys [elements list-links eavt entity]}]
  (-> (reduce (fn [agg {:keys [attribute value]}]
                (assoc! agg attribute (-edn {:elements   elements
                                             :list-links list-links
                                             :eavt       eavt
                                             :entity     value})))
              (some-> elements (get-in [entity :value]) transient)
              (avl/subrange eavt
                            >= (interpret/->Element entity nil nil)
                            <  (interpret/->Element (core/successor-id entity) nil nil)))
      persistent!
      (vary-meta assoc :converge/id entity)))

(defmethod -edn :vec
  [{:keys [elements list-links eavt entity]}]
  (let [attrs (persistent!
               (reduce (fn [agg {:keys [attribute value]}]
                         (assoc! agg attribute value))
                       (transient {})
                       (avl/subrange eavt
                                     >= (interpret/->Element entity nil nil)
                                     <  (interpret/->Element (core/successor-id entity) nil nil))))]
    (loop [ins (get list-links entity)
           ret (some-> elements (get-in [entity :value]) transient)
           idx []]
      (if (= ins interpret/list-end-sigil)
        (some-> ret
                persistent!
                (vary-meta assoc :converge/id entity :converge/insertions idx))
        (let [[next-ret next-idx]
              (if-let [value (get attrs ins)]
                [(conj! ret (-edn {:elements   elements
                                   :list-links list-links
                                   :eavt       eavt
                                   :entity     value}))
                 (conj idx ins)]
                [ret idx])]
          (recur (get list-links ins) next-ret next-idx))))))

;; TODO: build metadata index of (hash element) -> value id
(defmethod -edn :set
  [{:keys [elements list-links eavt entity]}]
  (-> (reduce (fn [agg {:keys [attribute]}]
                (conj! agg (-edn {:elements   elements
                                  :list-links list-links
                                  :eavt       eavt
                                  :entity     attribute})))
              (some-> elements (get-in [entity :value]) transient)
              (avl/subrange eavt
                            >= (interpret/->Element entity nil nil)
                            <  (interpret/->Element (core/successor-id entity) nil nil)))
      persistent!
      (vary-meta assoc :converge/id entity)))

(defmethod -edn :lst
  [{:keys [elements list-links eavt entity]}]
  (let [attrs (persistent!
               (reduce (fn [agg {:keys [attribute value]}]
                         (assoc! agg attribute value))
                       (transient {})
                       (avl/subrange eavt
                                     >= (interpret/->Element entity nil nil)
                                     <  (interpret/->Element (core/successor-id entity) nil nil))))]
    (loop [ins (get list-links entity)
           ret (get-in elements [entity :value])
           idx []]
      (if (= ins interpret/list-end-sigil)
        (vary-meta ret assoc :converge/id entity :converge/insertions idx)
        (let [[next-ret next-idx]
              (if-let [value (get attrs ins)]
                [(cons (-edn {:elements   elements
                              :list-links list-links
                              :eavt       eavt
                              :entity     value})
                       ret)
                 (conj idx ins)]
                [ret idx])]
          (recur (get list-links ins) next-ret next-idx))))))

(defn root-element-id
  [elements]
  (reduce-kv (fn [agg id {:keys [root?]}]
               (if root?
                 (max agg id)
                 agg))
             nil
             elements))

(defn edn
  "Transforms an converge.opset.interpret.Interpretation into an EDN value."
  [{:keys [elements list-links] :as _interpretation}]
  (-edn {:elements   elements
         :list-links list-links
         :eavt       (elements->eavt elements)
         :entity     (root-element-id elements)}))
