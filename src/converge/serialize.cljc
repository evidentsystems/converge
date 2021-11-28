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
  (:require [clojure.data.avl :as avl]
            [converge.core :as core]
            [converge.util :as util]
            [converge.opset.interpret :as interpret]
            [converge.opset.ref :as opset]
            [converge.editscript.ref :as editscript]))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

(defn read-avl-map
  [v]
  (into (avl/sorted-map) v))

(def read-id
  core/map->Id)

(def read-operation
  core/map->Op)

(def read-patch
  core/map->Patch)

(def read-element
  interpret/map->Element)

(def read-interpretation
  interpret/map->Interpretation)

(defn read-state
  [m]
  (core/map->ConvergentState
   {:opset  (:opset m)
    :dirty? true}))

(defn read-opset-convergent-ref
  [{:keys [state meta]}]
  (opset/->OpsetConvergentRef
   (util/uuid)
   state
   (util/queue)
   meta
   nil
   nil))

(defn read-editscript-convergent-ref
  [{:keys [state meta]}]
  (editscript/->EditscriptConvergentRef
   (util/uuid)
   state
   (util/queue)
   meta
   nil
   nil))

(defn write-avl-map
  [m]
  (into [] m))

(defn write-state
  [state]
  {:opset (:opset state)})

(defn write-ref
  [r]
  {:state (core/-state r)
   :meta  (meta r)})
