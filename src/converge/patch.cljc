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
(ns converge.patch
  (:require [clojure.data.avl :as avl]
            [editscript.core :as editscript]
            [editscript.edit :as edit]
            [converge.edn :as edn]
            [converge.interpret :as interpret]
            [converge.opset :as opset]
            [converge.util :as util]))


;;;; Editscript to Operations

(declare value-to-ops)
(defn map-to-value
  [value actor map-id start-id]
  (let [initial-ops
        (if (= opset/root-id map-id)
          [] ;; TODO: do we need this if we remove all special case init in convergent-ref?
          [[map-id (opset/make-map)]])]
    (:ops
     (reduce-kv (fn [{:keys [id] :as agg} k v]
                  (let [value-id  id
                        assign-id (opset/successor-id value-id)
                        value-ops (value-to-ops v actor value-id (opset/successor-id assign-id))]
                    (-> agg
                        (update :ops
                                into
                                (apply vector
                                       (first value-ops)
                                       [assign-id (opset/assign map-id k value-id)]
                                       (next value-ops)))
                        (assoc :id (opset/successor-id
                                    (if (next value-ops)
                                      (first (last value-ops))
                                      assign-id))))))
                {:id  start-id
                 :ops initial-ops}
                value))))

(defn sequential-to-value
  [value actor list-id start-id]
  (let [initial-ops
        (if (= opset/root-id list-id)
          [] ;; TODO: do we need this if we remove all special case init in convergent-ref?
          [[list-id (opset/make-list)]])]
    (:ops
     (reduce (fn [{:keys [id tail-id] :as agg} v]
               (let [insert-id id
                     value-id  (opset/successor-id insert-id)
                     assign-id (opset/successor-id value-id)
                     value-ops (value-to-ops v actor value-id (opset/successor-id assign-id))]
                 (-> agg
                     (update :ops
                             into
                             (apply vector
                                    [insert-id (opset/insert tail-id)]
                                    (first value-ops)
                                    [assign-id (opset/assign list-id insert-id value-id)]
                                    (next value-ops)))
                     (assoc :id (opset/successor-id
                                 (if (next value-ops)
                                   (first (last value-ops))
                                   assign-id))
                            :tail-id insert-id))))
             {:id      start-id
              :tail-id list-id
              :ops     initial-ops}
             value))))

(defn value-to-ops
  [value actor value-id start-id]
  (cond
    (map? value)
    (map-to-value value actor value-id start-id)

    (sequential? value)
    (sequential-to-value value actor value-id start-id)

    :else
    [[value-id (opset/make-value value)]]))

(defmulti -edit-to-ops
  "Returns a vector of tuples of [id op] that represent the given Editscript edit."
  (fn [edit _old-value entity _actor _id]
    [(if (map? entity) :map :list)
     (nth edit 1)]))

(defmethod -edit-to-ops :default
  [edit _old entity _actor _id]
  (throw
   (ex-info "Unknown edit operation"
            {:edit   edit
             :entity entity})))

(defmethod -edit-to-ops [:map :+]
  [[path _ value] old entity actor id]
  (let [entity-id (util/get-id entity)
        value-id  id
        assign-id (opset/successor-id value-id)
        value-ops (value-to-ops value actor value-id (opset/successor-id assign-id))]
    (apply vector
           (first value-ops)
           [assign-id (opset/assign entity-id (last path) value-id)]
           (next value-ops))))

(defmethod -edit-to-ops [:list :+]
  [[path _ value] old entity actor id]
  (let [entity-id (util/get-id entity)
        insert-id id
        value-id  (opset/successor-id insert-id)
        assign-id (opset/successor-id value-id)
        value-ops (value-to-ops value actor value-id (opset/successor-id assign-id))
        after-id  (or (some->> path last dec (util/get-insertion-id entity))
                      entity-id)]
    (apply vector
           [insert-id (opset/insert after-id)]
           (first value-ops)
           [assign-id (opset/assign entity-id insert-id value-id)]
           value-ops)))

(defmethod -edit-to-ops [:map :-]
  [[path :as edit] old entity actor id]
  [[id (opset/remove (util/get-id entity) (last path))]])

(defmethod -edit-to-ops [:list :-]
  [[path :as edit] old entity actor id]
  [[id (opset/remove (util/get-id entity) (util/get-insertion-id entity (last path)))]])

(defmethod -edit-to-ops [:map :r]
  [[path _ value :as edit] old entity actor id]
  (let [entity-id (util/get-id entity)]
    (cond
      (empty? old)
      (value-to-ops value actor opset/root-id id)

      (and (coll? value)
           (empty? value))
      (:ops
       (reduce (fn [{:keys [ops id] :as agg} attribute]
                 (-> agg
                     (update :ops conj [id (opset/remove entity-id attribute)])
                     (assoc :id (opset/successor-id id))))
               {:id  id
                :ops []}
               (keys old)))

      :else
      (let [value-id  id
            assign-id (opset/successor-id value-id)
            value-ops (value-to-ops value actor value-id (opset/successor-id assign-id))]
        (apply vector
               (first value-ops)
               [assign-id (opset/assign entity-id (last path) value-id)]
               (next value-ops))))))

(defmethod -edit-to-ops [:list :r]
  [[path _ value :as edit] old entity actor id]
  (let [entity-id (util/get-id entity)]
    (cond
      (empty? old)
      (value-to-ops value actor opset/root-id id)

      (and (coll? value)
           (empty? value))
      (:ops
       (reduce (fn [{:keys [ops id] :as agg} attribute]
                 (-> agg
                     (update :ops conj [id (opset/remove entity-id attribute)])
                     (assoc :id (opset/successor-id id))))
               {:id  id
                :ops []}
               (for [i (range (count old))]
                 (util/get-insertion-id old i))))

      :else
      (let [value-id  id
            insert-id (util/get-insertion-id entity (last path))
            assign-id (opset/successor-id value-id)
            value-ops (value-to-ops value actor value-id (opset/successor-id assign-id))]
        (apply vector
               (first value-ops)
               [assign-id (opset/assign entity-id insert-id value-id)]
               (next value-ops))))))

(defn edit-to-ops
  [[path :as edit] old actor id]
  (-edit-to-ops edit
                old
                (util/safe-get-in old (butlast path))
                actor
                id))

;; TODO: patch caches?
(defrecord Patch [ops])

(defn patch?
  [o]
  (instance? Patch o))

(defn make-patch
  [opset actor old-value new-value]
  (let [ops (some->> new-value
                     (editscript/diff old-value)
                     edit/get-edits
                     (reduce (fn [{:keys [ops id] :as agg} edit]
                               (let [new-ops (into ops
                                                   (edit-to-ops edit
                                                                old-value
                                                                actor
                                                                id))]
                                 (assoc agg
                                        :ops new-ops
                                        :id  (opset/next-id new-ops actor))))
                             {:ops (avl/sorted-map)
                              :id  (opset/next-id opset actor)})
                     :ops)]
    (if (seq ops) (->Patch ops))))
