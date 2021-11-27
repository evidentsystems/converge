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
(ns converge.opset.patch
  (:require [clojure.data.avl :as avl]
            [editscript.core :as e]
            [converge.core :as core]
            [converge.util :as util]
            [converge.opset.edn :as edn]
            [converge.opset.interpret :as interpret]
            [converge.opset.ops :as ops]))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;;; Value to Ops

(declare value-to-ops)
(defn map-to-value
  [value actor map-id start-id]
  (let [initial-ops
        (if (= core/root-id map-id)
          [] ;; TODO: do we need this if we remove all special case init in convergent-ref?
          [[map-id (ops/make-map)]])]
    (:ops
     (reduce-kv (fn [{:keys [id] :as agg} k v]
                  (let [value-id  id
                        assign-id (core/successor-id value-id)
                        value-ops (value-to-ops v actor value-id (core/successor-id assign-id))]
                    (-> agg
                        (update :ops
                                into
                                (apply vector
                                       (util/first-indexed value-ops)
                                       [assign-id (ops/assign map-id k value-id)]
                                       (next value-ops)))
                        (assoc :id (core/successor-id
                                    (if (next value-ops)
                                      (util/first-indexed (util/last-indexed value-ops))
                                      assign-id))))))
                {:id  start-id
                 :ops initial-ops}
                value))))

(defn sequential-to-value
  [value actor list-id start-id]
  (let [initial-ops
        (if (= core/root-id list-id)
          [] ;; TODO: do we need this if we remove all special case init in convergent-ref?
          [[list-id (ops/make-list)]])]
    (:ops
     (reduce (fn [{:keys [id tail-id] :as agg} v]
               (let [insert-id id
                     value-id  (core/successor-id insert-id)
                     assign-id (core/successor-id value-id)
                     value-ops (value-to-ops v actor value-id (core/successor-id assign-id))]
                 (-> agg
                     (update :ops
                             into
                             (apply vector
                                    [insert-id (ops/insert tail-id)]
                                    (util/first-indexed value-ops)
                                    [assign-id (ops/assign list-id insert-id value-id)]
                                    (next value-ops)))
                     (assoc :id (core/successor-id
                                 (if (next value-ops)
                                   (util/first-indexed (util/last-indexed value-ops))
                                   assign-id))
                            :tail-id insert-id))))
             {:id      start-id
              :tail-id list-id
              :ops     initial-ops}
             value))))

(defn value-to-ops
  [value actor value-id start-id]
  (case (util/get-type value)
    (:lst :vec)
    (sequential-to-value value actor value-id start-id)

    :map
    (map-to-value value actor value-id start-id)

    [[value-id (ops/make-value value)]]))

;;;; Edit to Ops

(defn get-id
  [o]
  (some-> o meta :converge/id))

(defn get-insertion-id
  [o n]
  (some-> o meta :converge/insertions (util/safe-get n)))

(defmulti -edit-to-ops
  "Returns a vector of tuples of [id op] that represent the given Editscript edit."
  (fn [edit _old-value entity _actor _id]
    [(nth edit 1)
     (util/get-type entity)]))

(defmethod -edit-to-ops :default
  [edit _old entity _actor _id]
  (throw
   (ex-info "Unknown edit operation"
            {:edit   edit
             :entity entity})))

(defmethod -edit-to-ops [:+ :map]
  [[path _ value] _old entity actor id]
  (let [entity-id (get-id entity)
        value-id  id
        assign-id (core/successor-id value-id)
        value-ops (value-to-ops value actor value-id (core/successor-id assign-id))]
    (apply vector
           (util/first-indexed value-ops)
           [assign-id (ops/assign entity-id (util/last-indexed path) value-id)]
           (next value-ops))))

(defn add-to-sequence
  [path value entity actor id]
  (let [entity-id (get-id entity)
        insert-id id
        value-id  (core/successor-id insert-id)
        assign-id (core/successor-id value-id)
        value-ops (value-to-ops value actor value-id (core/successor-id assign-id))
        after-id  (or (some->> path util/last-indexed dec (get-insertion-id entity))
                      entity-id)]
    (apply vector
           [insert-id (ops/insert after-id)]
           (util/first-indexed value-ops)
           [assign-id (ops/assign entity-id insert-id value-id)]
           value-ops)))

(defmethod -edit-to-ops [:+ :vec]
  [[path _ value] _old entity actor id]
  (add-to-sequence path value entity actor id))

(defmethod -edit-to-ops [:+ :lst]
  [[path _ value] _old entity actor id]
  (add-to-sequence path value entity actor id))

(defmethod -edit-to-ops [:- :map]
  [[path] _old entity _actor id]
  [[id (ops/remove (get-id entity) (util/last-indexed path))]])

(defn remove-from-sequence
  [path entity id]
  [[id (ops/remove (get-id entity) (get-insertion-id entity (util/last-indexed path)))]])

(defmethod -edit-to-ops [:- :vec]
  [[path] _old entity _actor id]
  (remove-from-sequence path entity id))

(defmethod -edit-to-ops [:- :lst]
  [[path] _old entity _actor id]
  (remove-from-sequence path entity id))

(defmethod -edit-to-ops [:r :map]
  [[path _ value] old entity actor id]
  (let [entity-id (get-id entity)]
    (cond
      (empty? old)
      (value-to-ops value actor core/root-id id)

      (and (coll? value)
           (empty? value))
      (:ops
       (reduce (fn [{:keys [id] :as agg} attribute]
                 (-> agg
                     (update :ops conj [id (ops/remove entity-id attribute)])
                     (assoc :id (core/successor-id id))))
               {:id  id
                :ops []}
               (keys old)))

      :else
      (let [value-id  id
            assign-id (core/successor-id value-id)
            value-ops (value-to-ops value actor value-id (core/successor-id assign-id))]
        (apply vector
               (util/first-indexed value-ops)
               [assign-id (ops/assign entity-id (util/last-indexed path) value-id)]
               (next value-ops))))))

(defn replace-in-sequence
  [path value old entity actor id]
  (let [entity-id (get-id entity)]
    (cond
      (empty? old)
      (value-to-ops value actor core/root-id id)

      (and (coll? value)
           (empty? value))
      (:ops
       (reduce (fn [{:keys [id] :as agg} attribute]
                 (-> agg
                     (update :ops conj [id (ops/remove entity-id attribute)])
                     (assoc :id (core/successor-id id))))
               {:id  id
                :ops []}
               (for [i (range (count old))]
                 (get-insertion-id old i))))

      :else
      (let [value-id  id
            insert-id (get-insertion-id entity (util/last-indexed path))
            assign-id (core/successor-id value-id)
            value-ops (value-to-ops value actor value-id (core/successor-id assign-id))]
        (apply vector
               (util/first-indexed value-ops)
               [assign-id (ops/assign entity-id insert-id value-id)]
               (next value-ops))))))

(defmethod -edit-to-ops [:r :vec]
  [[path _ value] old entity actor id]
  (replace-in-sequence path value old entity actor id))

(defmethod -edit-to-ops [:r :lst]
  [[path _ value] old entity actor id]
  (replace-in-sequence path value old entity actor id))

(defn edit-to-ops
  [[path :as edit] old actor id]
  (-edit-to-ops edit
                old
                (util/safe-get-in old (util/safe-pop path))
                actor
                id))

(defn make-patch
  [opset interpretation actor old-value new-value]
  (let [interp (or interpretation (interpret/interpret opset))

        ops
        (some->> new-value
                 (e/diff old-value)
                 e/get-edits
                 (reduce (fn [{:keys [value id ops] :as agg} edit]
                           (let [new-ops    (into ops (edit-to-ops edit value actor id))
                                 new-interp (interpret/interpret interp new-ops)]
                             (assoc agg
                                    :value  (edn/edn new-interp)
                                    :id     (core/next-id new-ops actor)
                                    :ops    new-ops)))
                         {:value old-value
                          :id    (core/next-id opset actor)
                          :ops   (avl/sorted-map)})
                 :ops)]
    (when (seq ops) (core/->Patch ops))))
