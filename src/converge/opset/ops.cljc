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
(ns converge.opset.ops
  (:refer-clojure :exclude [remove])
  (:require [converge.domain :as domain]))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

(def ^:const MAKE_MAP 1)
(def ^:const MAKE_VECTOR 2)
(def ^:const MAKE_SET 3)
(def ^:const MAKE_LIST 4)
(def ^:const MAKE_TEXT 5)
(def ^:const MAKE_KEY 6)
(def ^:const MAKE_VALUE 7)
(def ^:const INSERT 8)
(def ^:const ASSIGN 9)
(def ^:const REMOVE 10)

(defn make-map
  ([]
   (make-map false))
  ([root?]
   (domain/op MAKE_MAP (when root? {:root? true}))))

(defn make-vector
  ([]
   (make-vector false))
  ([root?]
   (domain/op MAKE_VECTOR (when root? {:root? true}))))

(defn make-set
  ([]
   (make-set false))
  ([root?]
   (domain/op MAKE_SET (when root? {:root? true}))))

(defn make-list
  ([]
   (make-list false))
  ([root?]
   (domain/op MAKE_LIST (when root? {:root? true}))))

(defn make-text
  ([]
   (make-text false))
  ([root?]
   (domain/op MAKE_TEXT (when root? {:root? true}))))

(defn make-key
  [value]
  (domain/op MAKE_KEY {:value value}))

(defn make-value
  ([value]
   (make-value value false))
  ([value root?]
   (domain/op MAKE_VALUE
            (merge {:value value}
                   (when root? {:root? true})))))

(defn insert
  [after]
  (assert (domain/id? after) "`after` must be an Id")
  (domain/op INSERT {:after after}))

(defn assign
  ([entity attribute]
   (assign entity attribute nil))
  ([entity attribute value]
   (assert (domain/id? entity)
           "`entity` must be an Id")
   (assert (domain/id? attribute)
           "`attribute` must be an Id")
   (assert (or (nil? value) (domain/id? value))
           "`value` must be either nil or an Id")
   (domain/op ASSIGN (merge {:entity entity :attribute attribute}
                          (when value {:value value})))))

(defn remove
  [entity attribute]
  (assert (domain/id? entity) "`entity` must be an Id")
  (domain/op REMOVE {:entity entity :attribute attribute}))
