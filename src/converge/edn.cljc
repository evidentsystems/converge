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
(ns converge.edn
  "API for interpreting an OpSet as EDN data."
  (:require [clojure.zip :as zip]
            [converge.opset :as opset]
            [converge.interpret :as interpret]
            [converge.util :as util]))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

(defn replace-id-values
  [this index]
  (cond
    (opset/id? this)
    (if (:key? (meta this))
      (vary-meta this dissoc :key?)
      (vary-meta (get index this)
                 assoc :converge/id this))

    (map-entry? this)
    (if (opset/id? (key this))
      (update this 0 vary-meta assoc :key? true)
      this)

    :else
    this))

(defn- build-list
  [head-id list-map list-links]
  (loop [prev       head-id
         list       []
         insertions []]
    (let [next    (get list-links prev)
          element (get list-map next)
          [next-list
           next-insertions]
          (if element
            [(conj list element)
             (conj insertions next)]
            [list insertions])]
      (if (= next interpret/list-end-sigil)
        {:value      next-list
         :insertions next-insertions}
        (recur next next-list next-insertions)))))

(defn element-adder
  [opset entities]
  (fn add-element
    [m {:keys [attribute value]}]
    (assoc m
           attribute
           (or (get-in opset [value :data :value])
               (get entities value)
               (case (get-in opset [value :action])
                 :make-map  {}
                 :make-list []
                 nil)))))

(defn assemble-value
  [opset entities list-links elements]
  (let [id  (-> elements first :entity)
        raw (reduce (element-adder opset entities)
                    {}
                    elements)]
    (if (some-> opset (get id) :action (= :make-list))
      (build-list id raw list-links)
      {:value raw})))

(defn interpretation-zip
  ([interpretation]
   (interpretation-zip interpretation opset/root-id))
  ([interpretation root-id]
   (let [elements (:elements interpretation)
         grouped  (group-by :entity (vals elements))]
     (zip/zipper
      (fn [node]
        (some #(contains? grouped %)
              (map :value (:elements node))))
      (fn [node]
        (for [{:keys [entity attribute value]}
              (:elements node)
              :when (contains? grouped value)]
          {:entity   value
           :elements (get grouped value)
           :path     (conj (:path node) attribute)}))
      (constantly nil) ;; TODO: if we need to "mutate" the zipper itself
      {:entity   root-id
       :elements (get grouped root-id)
       :path     []}))))

(defn index-path
  [path index list-links]
  (let [element-id (peek path)]
    (if (opset/id? element-id)
      (loop [i  0
             id (get-in index [(util/safe-pop path) :id])]
        (let [next (get list-links id)]
          (if (= next element-id)
            (conj (util/safe-pop path) i)
            (recur (inc i) next))))
      path)))

(defn assemble-values
  [opset {:keys [list-links] :as interpretation} root-id]
  (loop [loc          (interpretation-zip interpretation root-id)
         entities     {}
         index        {}
         return-trip? false]
    (if (nil? loc)
      {:value (get entities root-id)
       :index index}
      (let [node         (zip/node loc)
            path         (:path node)
            leaf?        (not (zip/branch? loc))
            return-trip? (or (zip/end? (zip/next loc))
                             return-trip?)
            {:keys [value insertions]}
            (assemble-value opset entities list-links (:elements node))]
        (recur (if return-trip?
                 (zip/prev loc)
                 (zip/next loc))
               (if (or leaf? return-trip?)
                 (assoc entities
                        (:entity node)
                        value)
                 entities)
               (assoc index
                      (index-path path index list-links)
                      (cond-> {:id (:entity node)}
                        insertions
                        (assoc :insertions insertions)))
               return-trip?)))))

(def root-index
  {[] {:id opset/root-id}})

(defn edn
  [opset]
  (cond
    (= (count opset) 0)
    nil

    (= (count opset) 1)
    {:value (case (some-> opset first val :action)
              :make-map  {}
              :make-list []
              (throw (ex-info "Invalid OpSet" {:opset opset})))
     :index root-index}

    :else
    (let [interpretation (interpret/interpret opset)]
      (assemble-values opset interpretation opset/root-id))))

(comment

  (require '[clojure.zip :as zip])

  (def a {:empty-m {}
          :empty-l []
          :a       :key
          :another {:nested {:key [1 2 3]}}
          :a-list  [:foo "bar" 'baz {:nested :inalist}]
          :a-set   #{1 2 3 4 5}})

  (def o (atom (opset/opset opset/root-id (opset/make-map))))
  (def i (opset/make-id))

  (swap! o into (opset/ops-from-diff @o (:actor i) {} root-index a))

  (def interpretation (interpret/interpret @o))
  (assemble-values @o interpretation opset/root-id)

  )
