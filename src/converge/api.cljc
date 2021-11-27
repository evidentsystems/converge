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
  (:require [converge.core :as core]
            [converge.util :as util]
            converge.opset.ref
            converge.editscript.ref
            ;; TODO: remove the following once squash/merge supports multiple backends
            [converge.opset.edn :as edn]
            [converge.opset.interpret :as interpret]))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

(def default-backend :editscript)

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
  [initial-value & {:keys [actor backend] :as options}]
  (assert (or (nil? actor) (uuid? actor))
          "Option `:actor`, if provided, must be a UUID")
  (let [r (core/make-ref (assoc options
                                :actor (or actor (util/uuid))
                                :backend (or backend default-backend)
                                :initial-value initial-value))]
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
  [opset & {:keys [actor backend] :as options}]
  (assert (or (nil? actor) (uuid? actor))
          "Option `:actor`, if provided, must be a UUID")
  ;; TODO: assertions ensuring valid opset
  (let [r (core/make-ref-from-ops
           (assoc options
                  :actor (or actor (util/uuid))
                  :backend (or backend default-backend)
                  :ops (into (core/opset) opset)))]
    @r
    r))

(defn convergent?
  [o]
  (satisfies? core/ConvergentRef o))

(defn actor
  [cr]
  (core/-actor cr))

(defn set-actor!
  [cr actor]
  (core/-set-actor! cr actor)
  cr)

(defn opset
  [cr]
  (core/-opset cr))

;; TODO: support multiple backends
(defn merge!
  [cr other]
  (let [patch (cond
                (nil? other)
                nil

                (convergent? other)
                (core/->Patch (opset other))

                (core/patch? other)
                other

                :else
                (throw (ex-info "Cannot merge! this object into convergent reference"
                                {:ref    cr
                                 :object other})))]
    (when patch
      (core/-apply-state! cr (core/-state-from-patch cr patch)))
    cr))

;; TODO: support multiple backends
(defn squash!
  [cr other]
  (let [additional-ops
        (cond
          (nil? other)
          nil

          (convergent? other)
          (opset other)

          (core/patch? other)
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
  (core/-peek-patches cr))

(defn pop-patches!
  [cr]
  (let [p (peek-patches cr)]
    (core/-pop-patches! cr)
    p))

(defn snapshot-ref
  "Creates a new reference which is a snapshot of the given reference,
  having a single `snapshot` operation in its opset."
    [cr & {:keys [actor backend] :as options}]
    (let [r  (core/make-snapshot-ref
              (assoc options
                     :actor (or actor (core/-actor cr))
                     :backend (or backend default-backend)
                     :state (core/-state cr)))]
      @r
      r))
