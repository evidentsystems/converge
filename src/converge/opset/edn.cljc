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
    (some->> entity
             (get elements)
             util/get-type))
  :default ::default)

(defmethod -edn ::default
  [{:keys [elements entity]}]
  (get elements entity))

(defmethod -edn :map
  [{:keys [elements list-links eavt entity]}]
  (let [container (get elements entity)]
    (-> (reduce (fn [agg {:keys [attribute value]}]
                  (assoc! agg attribute (-edn {:elements   elements
                                               :list-links list-links
                                               :eavt       eavt
                                               :entity     value})))
                (transient container)
                (avl/subrange eavt
                              >= (interpret/->Element entity nil nil)
                              <  (interpret/->Element (core/successor-id entity) nil nil)))
        persistent!
        (vary-meta assoc :converge/id entity))))

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
           ret (transient (get elements entity))
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

(defmethod -edn :set
  [{:keys [elements list-links eavt entity]}]
  (let [container (get elements entity)]
    (-> (reduce (fn [agg {:keys [attribute]}]
                  (conj! agg (-edn {:elements   elements
                                    :list-links list-links
                                    :eavt       eavt
                                    :entity         attribute})))
                (transient container)
                (avl/subrange eavt
                              >= (interpret/->Element entity nil nil)
                              <  (interpret/->Element (core/successor-id entity) nil nil)))
        persistent!
        (vary-meta assoc :converge/id entity))))

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
           ret (get elements entity)
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

(defn edn
  "Transforms an converge.opset.interpret.Interpretation into an EDN value."
  [{:keys [elements list-links] :as _interpretation}]
  (-edn {:elements   elements
         :list-links list-links
         :eavt       (elements->eavt elements)
         :entity     core/root-id}))

(comment

  (def l (converge.core/log core/root-id (converge.opset.ops/make-map)))

  (def p (converge.opset.patch/make-patch l nil (util/uuid) {} user/model))

  (def l* (merge l (:ops p)))

  (def i (interpret/interpret l*))

  (keys i)

  (get (:elements i) core/root-id)

  (edn i)

  )
