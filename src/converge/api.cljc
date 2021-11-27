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
(ns converge.api
  "The public API of the Converge library, exposing a convergent
  reference type that acts like a Clojure Atom, with additional
  functions for comparing, merging, and patching these convergent
  refs."
  (:refer-clojure :exclude [ref])
  (:require [converge.edn :as edn]
            [converge.interpret :as interpret]
            [converge.opset :as opset]
            [converge.patch :as patch]
            [converge.ref :as ref]
            [converge.util :as util]))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;; TODO: add note to docstring about our special top-level type
;; validation logic
(defn ref
  "Creates and returns a ConvergentRef with an initial value of `x` and
  zero or more options (in any order):

  :actor a UUID

  :meta metadata-map

  :validator validate-fn

  The actor uniquely identifies the origin of all changes made on this
  convergent ref, and must be globally unique for all sites/users of the
  underlying CRDT. If not provided, a random actor UUID will be
  generated. If metadata-map is supplied, it will become the metadata
  on the atom. validate-fn must be nil or a side-effect-free fn of one
  argument, which will be passed the intended new state on any state
  change. If the new state is unacceptable, the validate-fn should
  return false or throw an Error.  If either of these error conditions
  occur, then the value of the atom will not change."
  [initial-value & {:keys [actor meta validator] :as _options}]
  (assert (or (nil? actor) (uuid? actor))
          "Option `:actor`, if provided, must be a UUID")
  (let [actor* (or actor (util/uuid))

        r
        (cond
          (map? initial-value)
          (ref/->ConvergentRef actor*
                               (ref/->ConvergentState
                                (opset/opset opset/root-id (opset/make-map))
                                nil
                                nil
                                true)
                               (util/queue)
                               meta
                               validator
                               nil)

          (vector? initial-value)
          (ref/->ConvergentRef actor*
                               (ref/->ConvergentState
                                (opset/opset opset/root-id (opset/make-list))
                                nil
                                nil
                                true)
                               (util/queue)
                               meta
                               validator
                               nil)

          :else
          (throw (ex-info "The initial value of a convergent ref must be either a map or a vector."
                          {:initial-value initial-value})))]
    @r
    (reset! r initial-value)
    r))

(defn ref-from-opset
  "Creates and returns a ConvergentRef from the given `opset` and zero
  or more options (in any order):

  :actor a UUID

  :meta metadata-map

  :validator validate-fn

  The actor uniquely identifies the origin of all changes made on this
  convergent ref, and must be globally unique for all sites/users of
  the underlying CRDT. If not provided, a random actor UUID will be
  generated. If metadata-map is supplied, it will become the metadata
  on the atom. validate-fn must be nil or a side-effect-free fn of one
  argument, which will be passed the intended new state on any state
  change. If the new state is unacceptable, the validate-fn should
  return false or throw an Error.  If either of these error conditions
  occur, then the value of the atom will not change."
  [opset & {:keys [actor meta validator] :as _options}]
  (assert (or (nil? actor) (uuid? actor))
          "Option `:actor`, if provided, must be a UUID")
  ;; TODO: assertions ensuring valid opset
  (let [opset*         (into (opset/opset) opset)
        actor*         (or actor (util/uuid))
        r              (ref/->ConvergentRef actor*
                                            (ref/->ConvergentState opset* nil nil true)
                                            (util/queue)
                                            meta
                                            validator
                                            nil)]
    @r
    r))

(defn convergent?
  [o]
  (satisfies? ref/IConvergent o))

(defn actor
  [cr]
  (ref/-actor cr))

(defn set-actor!
  [cr actor]
  (ref/-set-actor! cr actor)
  cr)

(defn opset
  [cr]
  (ref/-opset cr))

(defn merge!
  [cr other]
  (let [patch (cond
                (nil? other)
                nil

                (convergent? other)
                (patch/->Patch (opset other))

                (patch/patch? other)
                other

                :else
                (throw (ex-info "Cannot merge! this object into convergent reference"
                                {:ref    cr
                                 :object other})))]
    (when patch
      (ref/-apply-state! cr (ref/-state-from-patch cr patch)))
    cr))

(defn squash!
  [cr other]
  (let [additional-ops
        (cond
          (nil? other)
          nil

          (convergent? other)
          (opset other)

          (patch/patch? other)
          (:ops other)

          :else
          (throw (ex-info "Cannot merge! this object into convergent reference"
                          {:ref    cr
                           :object other})))]
    (when-not (nil? additional-ops)
      (reset! cr (edn/edn (interpret/interpret (merge (opset cr) additional-ops)))))
    cr))

(defn peek-patches
  [cr]
  (ref/-peek-patches cr))

(defn pop-patches!
  [cr]
  (let [p (peek-patches cr)]
    (ref/-pop-patches! cr)
    p))

(defn snapshot-ref
  "Creates a new reference which is a snapshot of the given reference,
  having a single `snapshot` operation in its opset."
  [cr & {:keys [actor meta validator] :as _options}]
  (let [{o :opset
         i* :interpretation}
        (ref/-state cr)

        a  (or actor (ref/-actor cr))
        id (opset/latest-id o)
        i  (or i* (interpret/interpret o))
        r  (ref/->ConvergentRef
            a
            (ref/->ConvergentState (opset/opset
                                    (opset/successor-id id a)
                                    (opset/snapshot id i))
                                   i
                                   nil
                                   true)
            (util/queue)
            meta
            validator
            nil)]
    @r
    r))
