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

(defn write-id
  [id]
  [(:actor id) (:counter id)])

(defn read-id
  [[actor counter]]
  (domain/->Id actor counter))

(defn write-operation
  [op]
  [(:action op) (:data op)])

(defn read-operation
  [[action data]]
  (domain/->Op action data))

(defn write-patch
  [patch]
  {:ops    (:ops patch)
   :source (:source patch)})

(def read-patch
  domain/map->Patch)

(defn write-clock
  [clock]
  {:source (:source clock)
   :clock  (:clock clock)})

(def read-clock
  domain/map->Clock)

(defn write-element
  [element]
  [(:entity element)
   (:attribute element)
   (:value element)
   (:id element)])

(defn read-element
  [[entity attribute value id]]
  (interpret/->Element entity attribute value id))

(defn write-interpretation
  [interpretation]
  (into {}
        (-> interpretation
            (select-keys [:elements :list-links :entities :keys :values])
            (update :elements vec))))

(def read-interpretation
  interpret/make-interpretation)

(defn write-state
  [state]
  (-> state :log vec))

(defn read-state
  [log]
  (domain/make-state
   {:log    log
    :dirty? true}))

(defn write-ref
  [r]
  {:meta  (meta r)
   :state (domain/-state r)})

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
