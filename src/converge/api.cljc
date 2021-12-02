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
            converge.editscript.ref))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

(def default-backend :editscript)

(defn ref
  "Creates and returns a ConvergentRef with an initial value of `x` and
  zero or more options (in any order):

  :id a UUID

  :actor a UUID

  :meta metadata-map

  :validator validate-fn

  The `id` uniquely identifies this convergent reference across all
  peers. The `actor` uniquely identifies the origin of all changes made
  on this convergent ref, and must be globally unique for all
  sites/users of the underlying CRDT. If not provided, a random actor
  UUID will be generated. If metadata-map is supplied, it will become
  the metadata on the atom. validate-fn must be nil or a
  side-effect-free fn of one argument, which will be passed the
  intended new state on any state change. If the new state is
  unacceptable, the validate-fn should return false or throw an Error.
  If either of these error conditions occur, then the value of the
  atom will not change."
  [initial-value & {:keys [id actor backend] :as options}]
  (assert (or (nil? id) (uuid? id))
          "Option `:id`, if provided, must be a UUID")
  (assert (or (nil? actor) (uuid? actor))
          "Option `:actor`, if provided, must be a UUID")
  (let [backend* (or backend default-backend)
        log      (core/log
                  (core/make-id nil)
                  (core/root-op (or id (util/uuid)) backend*))
        r        (core/make-ref (assoc options
                                       :log log
                                       :actor (or actor (util/uuid))
                                       :backend backend*
                                       :initial-value initial-value))]
    @r
    r))

(defn ref-from-ops
  "Creates and returns a ConvergentRef from the given `ops` and zero
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
  [ops & {:keys [actor] :as options}]
  (assert (or (nil? actor) (uuid? actor))
          "Option `:actor`, if provided, must be a UUID")
  ;; TODO: assertions ensuring valid operation ops
  (let [r (core/make-ref-from-ops
           (assoc options
                  :actor (or actor (util/uuid))
                  :ops (into (core/log) ops)))]
    @r
    r))

(defn snapshot-ref
  "Creates a new reference which is a snapshot of the given reference,
  having a single `snapshot` operation in its log."
  [cr & {:keys [actor] :as options}]
  (let [r  (core/make-snapshot-ref
            (assoc options
                   :actor (or actor (core/-actor cr))
                   :state (core/-state cr)))]
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

(defn log
  [cr]
  (core/-log cr))

(defn ref-id
  [cr]
  (-> cr log core/ref-id-from-log))

(defn merge!
  [cr other]
  (assert (convergent? cr) "Destination must be a ConvergentRef")
  (let [cr-id (ref-id cr)
        patch (cond
                (nil? other)
                nil

                (and (= (type cr) (type other))
                     (= cr-id (ref-id other)))
                (core/->Patch (ref-id other) (log other))

                (and (core/patch? other)
                     (= cr-id (:source other)))
                other

                :else
                (throw (ex-info "Cannot merge! this object into convergent reference"
                                {:ref    cr
                                 :object other})))]
    (when patch
      (core/-apply-state! cr (core/-state-from-patch cr patch)))
    cr))

(defn squash!
  [cr other]
  (assert (convergent? cr) "Destination must be a ConvergentRef")
  (let [cr-id (ref-id cr)

        additional-ops
        (cond
          (nil? other)
          nil

          (and (= (type cr) (type other))
               (= cr-id (ref-id other)))
          (log other)

          (and (core/patch? other)
               (= cr-id (:source other)))
          (:ops other)

          :else
          (throw (ex-info "Cannot squash! this object into convergent reference"
                          {:ref    cr
                           :object other})))]
    (when-not (nil? additional-ops)
      (reset! cr (core/-value-from-ops cr (merge (log cr) additional-ops))))
    cr))

(defn peek-patches
  [cr]
  (core/-peek-patches cr))

(defn pop-patches!
  [cr]
  (let [p (peek-patches cr)]
    (core/-pop-patches! cr)
    p))
