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
(ns converge.opset
  "Data types and functions to implement and manage OpSets: lamport-like `Id`s,
  `Op`(eration)s as per section 3.1 of the
  [OpSet paper](https://arxiv.org/pdf/1805.04263.pdf), and `OpSet`s or
  totally ordered maps of id -> op."
  (:refer-clojure :exclude [remove])
  (:require [clojure.data.avl :as avl]
            [converge.util :as util])
  #?(:clj (:import java.util.UUID)))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;;; Identifiers

(declare id?)

(defrecord Id [^UUID actor ^long counter]
  #?(:clj  Comparable
     :cljs IComparable)
  (#?(:clj  compareTo
      :cljs -compare)
    [_ other]
    (assert (id? other))
    (compare
     [counter actor]
     [(:counter other) (:actor other)])))

(defn make-id
  ([]
   (make-id (util/uuid)))
  ([actor]
   (make-id actor 0))
  ([actor counter]
   (assert (nat-int? counter) "The `counter` of an Id must be an integer")
   (assert (or (nil? actor) (uuid? actor)) "The `actor` of an Id must be a UUID")
   (->Id actor counter)))

(defn id?
  [o]
  (instance? Id o))

(def root-id
  (make-id nil 0))

(defn successor-id
  ([id]
   (successor-id id (:actor id)))
  ([{:keys [counter]} actor]
   (make-id actor (inc counter))))

(defn latest-id
  [opset]
  (some-> opset util/last-indexed key))

(defn next-id
  [opset actor]
  (if-let [latest (latest-id opset)]
    (successor-id latest actor)
    root-id))

;;;; Operations

(def MAKE_MAP 0)
(def MAKE_VECTOR 1)
(def MAKE_SET 2)
(def MAKE_LIST 3)
(def MAKE_VALUE 4)
(def INSERT 5)
(def ASSIGN 6)
(def REMOVE 7)
(def SNAPSHOT 8)

(defrecord Op [^long action data])

(defn op
  ([action]
   (op action nil))
  ([action data]
   (assert (integer? action) "The `action` of an Op must be an integer")
   (assert (or (nil? data) (map? data)) "The `data` of an Op, if provided, must be a map")
   (->Op action data)))

(defn make-map
  []
  (op MAKE_MAP))

(defn make-vector
  []
  (op MAKE_VECTOR))

(defn make-set
  []
  (op MAKE_SET))

(defn make-list
  []
  (op MAKE_LIST))

(defn make-value
  [value]
  (op MAKE_VALUE {:value value}))

(defn insert
  [after]
  (assert (id? after) "`after` must be an Id")
  (op INSERT {:after after}))

(defn assign
  [entity attribute value]
  (assert (id? entity) "`entity` must be an Id")
  (op ASSIGN {:entity entity :attribute attribute :value value}))

(defn remove
  [entity attribute]
  (assert (id? entity) "`entity` must be an Id")
  (op REMOVE {:entity entity :attribute attribute}))

(defn snapshot
  [as-of interpretation]
  (assert (id? as-of) "`as-of` must be an Id")
  (op SNAPSHOT {:as-of as-of :interpretation interpretation}))

;;;; OpSets

(defn opset
  "An opset is a sorted map of Id -> Op"
  ([]
   (avl/sorted-map))
  ([& id-ops]
   (apply avl/sorted-map id-ops)))
