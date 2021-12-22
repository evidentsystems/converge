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
(ns converge.transit
  (:require clojure.data.avl
            [converge.serialize :as serialize]
            [cognitect.transit :as transit])
  (:import [clojure.data.avl AVLMap AVLSet]
           [converge.domain Id Op Patch ConvergentState Clock]
           [converge.editscript.ref EditscriptConvergentRef]
           [converge.opset.interpret Element Interpretation]
           [converge.opset.ref OpsetConvergentRef]))

(defn tagged-map-value
  [rec]
  (transit/tagged-value "map" rec))

(def read-handlers
  {"avl/map"              (transit/read-handler serialize/read-avl-map)
   "avl/set"              (transit/read-handler serialize/read-avl-set)
   "converge/id"          (transit/read-handler serialize/read-id)
   "converge/op"          (transit/read-handler serialize/read-operation)
   "converge/patch"       (transit/read-handler serialize/read-patch)
   "converge/state"       (transit/read-handler serialize/read-state)
   "converge/clock"       (transit/read-handler serialize/read-clock)
   "opset/element"        (transit/read-handler serialize/read-element)
   "opset/interpretation" (transit/read-handler serialize/read-interpretation)
   "opset/ref"            (transit/read-handler serialize/read-opset-convergent-ref)
   "editscript/ref"       (transit/read-handler serialize/read-editscript-convergent-ref)})

(def write-handlers
  {AVLMap (transit/write-handler (constantly "avl/map") serialize/write-avl-map)
   AVLSet (transit/write-handler (constantly "avl/set") serialize/write-avl-set)

   Id              (transit/write-handler (constantly "converge/id") tagged-map-value)
   Op              (transit/write-handler (constantly "converge/op") tagged-map-value)
   Patch           (transit/write-handler (constantly "converge/patch") serialize/write-patch)
   ConvergentState (transit/write-handler (constantly "converge/state") serialize/write-state)
   Clock           (transit/write-handler (constantly "converge/state") tagged-map-value)

   Element            (transit/write-handler (constantly "opset/element") tagged-map-value)
   Interpretation     (transit/write-handler (constantly "opset/interpretation") serialize/write-interpretation)
   OpsetConvergentRef (transit/write-handler (constantly "opset/ref") serialize/write-ref)

   EditscriptConvergentRef (transit/write-handler (constantly "editscript/ref") serialize/write-ref)})
