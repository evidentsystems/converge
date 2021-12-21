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
  (:require [converge.domain :as domain]
            converge.opset.ref
            converge.editscript.ref))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

(def backends #{:opset :editscript})
(def default-backend :opset)

(def random-actor domain/random-id-member)

(defn ref
  "Creates and returns a ConvergentRef with an initial value of `x` and
  zero or more options (in any order):

  :id a UUID

  :actor an integer between 1 and Javascript Number/MAX_SAFE_INTEGER

  :backend one of `backends`

  :meta metadata-map

  :validator validate-fn

  The `id` uniquely identifies this convergent reference across all
  peers. The `actor` uniquely identifies the origin of all changes made
  on this convergent ref, and must be globally unique for all
  sites/users of the underlying CRDT. If not provided, a random actor
  will be generated. If metadata-map is supplied, it will become
  the metadata on the atom. validate-fn must be nil or a
  side-effect-free fn of one argument, which will be passed the
  intended new state on any state change. If the new state is
  unacceptable, the validate-fn should return false or throw an Error.
  If either of these error conditions occur, then the value of the
  atom will not change."
  [initial-value & {:keys [id actor backend] :as options}]
  (assert (or (nil? id) (uuid? id))
          "Option `:id`, if provided, must be a UUID")
  (assert (or (nil? actor) (domain/valid-id-member? actor))
          "Option `:actor`, if provided, must be an integer between 1 and Javascript Number/MAX_SAFE_INTEGER")
  (assert (or (nil? backend) (backends backend))
          (str "Option `:backend`, if provided, must be one of " backends))
  (let [actor* (or actor (random-actor))
        backend* (or backend default-backend)
        log      (domain/make-log
                  domain/root-id
                  (domain/root-op (or id (domain/uuid)) actor* backend*))
        r        (domain/make-ref (assoc options
                                         :log log
                                         :actor actor*
                                         :backend backend*
                                         :initial-value initial-value))]
    @r
    r))

(defn ref-from-ops
  "Creates and returns a ConvergentRef from the given `ops` and zero
  or more options (in any order):

  :actor an integer between 1 and Javascript Number/MAX_SAFE_INTEGER

  :meta metadata-map

  :validator validate-fn

  The actor uniquely identifies the origin of all changes made on this
  convergent ref, and must be globally unique for all sites/users of
  the underlying CRDT. If not provided, a random actor will be
  generated. If metadata-map is supplied, it will become the metadata
  on the atom. validate-fn must be nil or a side-effect-free fn of one
  argument, which will be passed the intended new state on any state
  change. If the new state is unacceptable, the validate-fn should
  return false or throw an Error.  If either of these error conditions
  occur, then the value of the atom will not change."
  [ops & {:keys [actor] :as options}]
  (assert (or (nil? actor) (domain/valid-id-member? actor))
          "Option `:actor`, if provided, must be an integer between 1 and Javascript Number/MAX_SAFE_INTEGER")
  ;; TODO: assertions ensuring valid operation log
  (let [log (into (domain/make-log) ops)
        r   (domain/make-ref-from-ops
             (assoc options
                    :backend (-> log domain/ref-root-data-from-log :backend)
                    :actor (or actor (random-actor))
                    :ops log))]
    @r
    r))

(defn convergent?
  [o]
  (satisfies? domain/ConvergentRef o))

(defn ref-actor
  [cr]
  (domain/-actor cr))

(defn set-actor!
  [cr actor]
  (assert (domain/valid-id-member? actor)
          "`actor` must be an integer between 1 and Javascript Number/MAX_SAFE_INTEGER")
  (domain/-set-actor! cr actor)
  cr)

(defn ref-log
  [cr]
  (domain/-log cr))

(defn ref-id
  [cr]
  (-> cr ref-log domain/ref-root-data-from-log :id))

(defn ref-creator
  [cr]
  (-> cr ref-log domain/ref-root-data-from-log :creator))

(defn ref-backend
  [cr]
  (-> cr ref-log domain/ref-root-data-from-log :backend))

(defn merge!
  [cr other]
  (assert (convergent? cr) "Destination must be a ConvergentRef")
  (let [cr-id (ref-id cr)
        patch (cond
                (nil? other)
                nil

                (and (= (type cr) (type other))
                     (= cr-id (ref-id other)))
                (domain/->Patch (ref-id other) (ref-log other))

                (and (domain/patch? other)
                     (= cr-id (:source other)))
                other

                :else
                (throw (ex-info "Cannot merge! this object into convergent reference"
                                {:ref    cr
                                 :object other})))]
    (when patch
      (domain/-apply-state! cr (domain/-state-from-patch cr patch)))
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

          (and (domain/patch? other)
               (= cr-id (:source other)))
          (:ops other)

          :else
          (throw (ex-info "Cannot squash! this object into convergent reference"
                          {:ref    cr
                           :object other})))]
    (when-not (nil? additional-ops)
      (reset! cr (domain/-value-from-ops cr (merge (ref-log cr) additional-ops))))
    cr))

(defn peek-patches
  [cr]
  (domain/-peek-patches cr))

(defn pop-patches!
  [cr]
  (let [p (peek-patches cr)]
    (domain/-pop-patches! cr)
    p))

(defn clock
  "Returns a vector clock (convergent.domain.Clock) for the given convergent ref."
  [cr]
  (when cr
    (domain/->Clock
     (ref-id cr)
     (persistent!
      (reduce (fn [clock id]
                (assoc! clock (:actor id) id))
              (transient {})
              (keys (domain/-log cr)))))))

(defn patch-from-clock
  "Provided the given clock's source matches the given convergent ref (or is an empty clock),
  returns a Patch of all ops included in this ref after the clock."
  [cr {:keys [source clock] :as _foreign-clock}]
  (if (or (= (ref-id cr) source)
          (empty? clock))
    (domain/->Patch
     (ref-id cr)
     (domain/log-ops-after-clock
      (domain/-log cr)
      clock))
    (throw
     (ex-info
      "Clock source doesn't match ref id!"
      {:clock-source source
       :ref-id (ref-id cr)}))))
