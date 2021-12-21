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
(ns converge.domain
  "Datatypes and functions implementing a serializable, Atom-like
  convergent reference type."
  #?(:cljs (:refer-clojure :exclude [uuid]))
  (:require [clojure.data.avl :as avl]
            [clojure.string :as string]
            [editscript.edit :as edit]
            #?(:cljs [uuid :as uuid]))
  #?(:clj (:import java.util.UUID)))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;;; Util

(defn uuid
  ([]
   #?(:clj  (UUID/randomUUID)
      :cljs (random-uuid)))
  ([uuid-str]
   (cond
     (uuid? uuid-str)
     uuid-str

     (string/blank? uuid-str)
     nil

     (string? uuid-str)
     #?(:clj
        (UUID/fromString uuid-str)
        :cljs
        (if (uuid/validate uuid-str)
          (cljs.core/uuid uuid-str)
          (throw (ex-info "invalid UUID string" {:uuid-str uuid-str}))))

     :else
     (throw (ex-info "cannot make UUID from object" {:object uuid-str})))))

(def get-type edit/get-type)

;; HT: https://github.com/juji-io/editscript/blob/ddb13130d16ae920d1ead8ae77b23c24a202e92e/src/editscript/patch.cljc#L18
(defn safe-get
  ([x p]
   (safe-get x p nil))
  ([x p not-found]
   (case (get-type x)
     (:map :vec :set) (get x p not-found)
     :lst             (nth x p not-found))))

(def lookup-sentinel
  #?(:clj  (Object.)
     :cljs (js-obj)))

(defn safe-get-in
  ([m ks]
   (safe-get-in m ks nil))
  ([m ks not-found]
   (loop [sentinel lookup-sentinel
          m        m
          ks       (seq ks)]
     (if ks
       (let [m (safe-get m (first ks) sentinel)]
         (if (identical? sentinel m)
           not-found
           (recur sentinel m (next ks))))
       m))))

(defn first-indexed
  [indexed]
  (nth indexed 0 nil))

(defn last-indexed
  [indexed]
  (nth indexed (dec (count indexed)) nil))

(defn safe-pop
  [indexed]
  (when-not (empty? indexed)
    (pop indexed)))

(defn queue
  [& elems]
  (into #?(:clj  clojure.lang.PersistentQueue/EMPTY
           :cljs (.-EMPTY PersistentQueue))
        elems))

;;;; API

;; TODO trim down to bare semantics, split other ops into different protocols
(defprotocol ConvergentRef
  (-actor [this]
    "Returns the current actor of this convergent ref")
  (-state [this]
    "Returns the current state (an instance of ConvergentState) of this convergent ref")
  (-set-actor! [this actor]
    "Sets this ref's actor to the given value")
  (-log [this]
    "Returns this convergent ref's log")
  (-make-patch [this new-value]
    "Returns a Patch representing the changes necessary to reset from
    the current value to the provided value.")
  (-state-from-patch [this patch]
    "Returns a new ConvergentState that is the result of applying the
    given patch to the current state.")
  (-apply-state! [this state]
    "Sets this ref's state to the given value and notifies watches. USE WITH CAUTION!")
  (-peek-patches [this]
    "Returns the next patch from this ref's queue of applied patches.")
  (-pop-patches! [this]
    "Mutates this ref's queue of applied patches as with pop, and
    returns the queue's new value.")
  (-value-from-ops [this ops]
    "Generates a value from the provided ops, not relying on any internal state."))

(defmulti make-ref :backend :default ::default)

(defmethod make-ref ::default
  [options]
  (throw (ex-info "Unknown ConvergentRef backend" options)))

(defmulti make-ref-from-ops :backend :default ::default)

(defmethod make-ref-from-ops ::default
  [options]
  (throw (ex-info "Unknown ConvergentRef backend" options)))

;;;; Implementation

(defn notify-w
  [this watches old-value new-value]
  (doseq [[k w] watches]
    (when w (w k this old-value new-value))))

(defn validate-reset
  [old-value new-value new-state patch]
  (when-not (= new-value (:value new-state))
    (throw (ex-info "Unsupported reference state" {:old-value old-value
                                                   :new-value new-value
                                                   :patch     patch
                                                   :new-state new-state}))))

;;;; Identifiers

(declare id?)

(def highest-id
  (reify
    #?(:clj  Comparable
       :cljs IComparable)
    (#?(:clj  compareTo
        :cljs -compare)
      [_ other]
      (if (id? other)
        1
        0))))

(def max-javascript-integer
  #?(:clj  (long (dec (Math/pow 2 53)))
     :cljs Number/MAX_SAFE_INTEGER))

(defn random-id-member
  "Generates a random integer between 1 and `max-javascript-integer`"
  []
  (inc (long (* (rand) (dec max-javascript-integer)))))

(defn valid-id-member?
  [i]
  (and (> i 0)
       (<= i max-javascript-integer)))

(declare id?)

(defrecord Id [^long actor ^long counter]
  #?(:clj  Comparable
     :cljs IComparable)
  (#?(:clj  compareTo
      :cljs -compare)
    [_ other]
    (cond
      (nil? other)
      1

      (identical? other highest-id)
      -1

      (id? other)
      (let [c (#?(:clj Long/compare :cljs -) counter (:counter other))]
        (if (zero? c)
          (#?(:clj Long/compare :cljs -) actor (:actor other))
          c))

      :else
      0)))

(def root-id
  (->Id 0 0))

(defn make-id
  ([]
   (make-id 1))
  ([actor]
   (make-id actor 1))
  ([actor counter]
   (assert (valid-id-member? counter)   "The `counter` of an Id must be an integer")
   (assert (valid-id-member? actor) "The `actor` of an Id must be an integer")
   (->Id actor counter)))

(defn id?
  [o]
  (instance? Id o))

(defn successor-id
  ([id]
   (successor-id id (:actor id)))
  ([{:keys [counter]} actor]
   (make-id actor (inc counter))))

(defn latest-id
  [log]
  (some-> log last-indexed key))

(defn next-id
  ([log] (next-id log nil))
  ([log actor]
   (if-let [latest (latest-id log)]
     (if actor
       (successor-id latest actor)
       (successor-id latest))
     (if actor
       (make-id actor)
       (throw (ex-info "Can't get the next-id of an empty log without an `actor`" {:log log}))))))

;;;; Operation Log

(def ^{:doc "An operation log is a sorted map of Id -> Op"}
  make-log avl/sorted-map)

(defrecord Op [^long action data])

(defn op
  ([action]
   (op action nil))
  ([action data]
   (assert (integer? action) "The `action` of an Op must be an integer")
   (assert (or (nil? data) (map? data)) "The `data` of an Op, if provided, must be a map")
   (->Op action data)))

(def ^:const ROOT 0)

(defn root-op
  [id actor backend]
  (op ROOT {:id id :creator actor :backend backend}))

(defn ref-root-data-from-log
  [log]
  (-> log first-indexed val :data))

;;;; Patch

(defrecord Patch [source ops])

(defn patch?
  [o]
  (instance? Patch o))

;;;; State

(defrecord ConvergentState [log cache value ^boolean dirty?])

(defn make-state
  [{:keys [log] :as state}]
  (map->ConvergentState
   (assoc state :log (into (avl/sorted-map) log))))

;;;; Clock

(defrecord Clock [source clock])

(defn log-ops-after-clock
  [log clock]
  (persistent!
   (reduce-kv (fn [ops {:keys [actor] :as id} op]
                (let [clock-id (get clock actor)]
                  (if (or
                       ;; the foreign clock doesn't contain any ops from this actor
                       (nil? clock-id)
                       ;; this op is later than the latest in the foreign clock for this actor
                       (pos? (compare id clock-id)))
                    (assoc! ops id op)
                    ops)))
              (transient {})
              log)))
