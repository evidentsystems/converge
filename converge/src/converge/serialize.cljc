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
(ns converge.serialize
  "Handlers for serializing to e.g. Transit."
  (:require [converge.domain :as domain]
            [converge.opset.interpret :as interpret]
            [converge.opset.ref :as opset]
            [converge.editscript.ref :as editscript]))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

(def read-id
  domain/map->Id)

(def read-operation
  domain/map->Op)

(def read-patch
  domain/map->Patch)

(def read-clock
  domain/map->Clock)

(def read-element
  interpret/map->Element)

(def read-interpretation
  interpret/make-interpretation)

(defn read-state
  [m]
  (domain/make-state
   {:log    (:log m)
    :dirty? true}))

(defn read-opset-convergent-ref
  [{:keys [state meta]}]
  (opset/->OpsetConvergentRef
   (domain/uuid)
   state
   (domain/queue)
   meta
   nil
   nil))

(defn read-editscript-convergent-ref
  [{:keys [state meta]}]
  (editscript/->EditscriptConvergentRef
   (domain/uuid)
   state
   (domain/queue)
   meta
   nil
   nil))

(defn write-patch
  [patch]
  {:ops    (:ops patch)
   :source (:source patch)})

(defn write-interpretation
  [interpretation]
  (-> interpretation
      (select-keys [:elements :list-links :entities :keys :values])
      (update :elements vec)))

(defn write-state
  [state]
  {:log (-> state :log vec)})

(defn write-ref
  [r]
  {:state (domain/-state r)
   :meta  (meta r)})
