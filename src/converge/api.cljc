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

(def backends #{:opset :editscript})
(def default-backend :editscript)

(defn ref
  "Creates and returns a ConvergentRef with an initial value of `x` and
  zero or more options (in any order):

  :id a UUID

  :actor a UUID

  :backend one of `backends`

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
  (assert (or (nil? backend) (backends backend))
          (str "Option `:backend`, if provided, must be one of " backends))
  (let [actor* (or actor (util/uuid))
        backend* (or backend default-backend)
        log      (core/make-log
                  (core/make-id)
                  (core/root-op (or id (util/uuid)) actor* backend*))
        r        (core/make-ref (assoc options
                                       :log log
                                       :actor actor*
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
  ;; TODO: assertions ensuring valid operation log
  (let [log (into (core/make-log) ops)
        r   (core/make-ref-from-ops
             (assoc options
                    :backend (-> log core/ref-root-data-from-log :backend)
                    :actor (or actor (util/uuid))
                    :ops log))]
    @r
    r))

(defn convergent?
  [o]
  (satisfies? core/ConvergentRef o))

(defn ref-actor
  [cr]
  (core/-actor cr))

(defn set-actor!
  [cr actor]
  (core/-set-actor! cr actor)
  cr)

(defn ref-log
  [cr]
  (core/-log cr))

(defn ref-id
  [cr]
  (-> cr ref-log core/ref-root-data-from-log :id))

(defn ref-creator
  [cr]
  (-> cr ref-log core/ref-root-data-from-log :creator))

(defn ref-backend
  [cr]
  (-> cr ref-log core/ref-root-data-from-log :backend))

(defn merge!
  [cr other]
  (assert (convergent? cr) "Destination must be a ConvergentRef")
  (let [cr-id (ref-id cr)
        patch (cond
                (nil? other)
                nil

                (and (= (type cr) (type other))
                     (= cr-id (ref-id other)))
                (core/->Patch (ref-id other) (ref-log other))

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
          (ref-log other)

          (and (core/patch? other)
               (= cr-id (:source other)))
          (:ops other)

          :else
          (throw (ex-info "Cannot squash! this object into convergent reference"
                          {:ref    cr
                           :object other})))]
    (when-not (nil? additional-ops)
      (reset! cr (core/-value-from-ops cr (merge (ref-log cr) additional-ops))))
    cr))

(defn peek-patches
  [cr]
  (core/-peek-patches cr))

(defn pop-patches!
  [cr]
  (let [p (peek-patches cr)]
    (core/-pop-patches! cr)
    p))
