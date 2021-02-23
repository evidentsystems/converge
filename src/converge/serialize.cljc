(ns converge.serialize
  "Handlers for serializing to e.g. Transit."
  (:require [clojure.data.avl :as avl]
            [converge.interpret :as interpret]
            [converge.opset :as opset]
            [converge.patch :as patch]
            [converge.ref :as ref]
            [converge.util :as util]))

(def read-id
  opset/map->Id)

(def read-operation
  opset/map->Op)

(def read-patch
  patch/map->Patch)

(def read-element
  interpret/map->Element)

(def read-interpretation
  interpret/map->Interpretation)

(defn read-state
  [m]
  (ref/map->ConvergentState
   {:opset  (:opset m)
    :dirty? true}))

(defn read-ref
  [{:keys [state meta]}]
  (ref/->ConvergentRef (util/uuid)
                       state
                       (util/queue)
                       meta
                       nil
                       nil))

(defn read-avl-map
  [v]
  (into (avl/sorted-map) v))

(defn write-state
  [state]
  {:opset (:opset state)})

(defn write-ref
  [r]
  {:state (ref/-state r)
   :meta  (meta r)})

(defn write-avl-map
  [m]
  (into [] m))
