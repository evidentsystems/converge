(ns converge.nippy
  (:require [converge.serialize :as serialize]
            [taoensso.nippy :as nippy])
  (:import [converge.domain Id Op Patch Clock ConvergentState]
           [converge.editscript.ref EditscriptConvergentRef]
           [converge.opset.interpret Element Interpretation]
           [converge.opset.ref OpsetConvergentRef]))

(set! *warn-on-reflection* true)

(defmacro extend-nippy
  [handler-data]
  (let [extentions
        (for [[t s write-fn read-fn] handler-data]
          `(do
             (nippy/extend-freeze
                 ~t ~s
                 [x# out#]
                 (nippy/freeze-to-out! out# (~write-fn x#)))

             (nippy/extend-thaw
              ~s
              [in#]
              (~read-fn (nippy/thaw-from-in! in#)))))]
    `(do ~@extentions)))

(extend-nippy
 [[Id :converge/id serialize/write-id serialize/read-id]
  [Op :converge/op serialize/write-operation serialize/read-operation]
  [Patch :converge/patch serialize/write-patch serialize/read-patch]
  [Clock :converge/clock serialize/write-clock serialize/read-clock]
  [ConvergentState :converge/state serialize/write-state serialize/read-state]
  [Element :opset/element serialize/write-element serialize/read-element]
  [Interpretation :opset/interpretation serialize/write-interpretation serialize/read-interpretation]
  [OpsetConvergentRef :opset/ref serialize/write-ref serialize/read-opset-convergent-ref]
  [EditscriptConvergentRef :editscript/ref serialize/write-ref serialize/read-editscript-convergent-ref]])

(comment

  (require '[clojure.java.io :as io]
           '[converge.api :as convergent]
           '[converge.opset.interpret :as interpret])

  (def r (convergent/ref {:foo :bar}))

  (nippy/freeze-to-file "ref.nippy" r)

  (def ref-from-read
    (nippy/thaw-from-file "ref.nippy"))

  (= (convergent/ref-log r)
     (convergent/ref-log ref-from-read))

  (def interpretation
    (interpret/interpret (convergent/ref-log r)))

  (nippy/freeze-to-file "interpretation.nippy" interpretation)

  (def interpretation-from-read
    (nippy/thaw-from-file "interpretation.nippy"))

  (= interpretation interpretation-from-read)

  (def clock (convergent/clock r))

  (nippy/freeze-to-file "clock.nippy" clock)

  (def clock-from-read
    (nippy/thaw-from-file "clock.nippy"))

  (= clock clock-from-read)

  (def r1 (convergent/ref-from-ops (convergent/ref-log r)))
  (swap! r assoc :quux :bar)
  (def patch (convergent/patch-from-clock r (convergent/clock r1)))

  (nippy/freeze-to-file "patch.nippy" patch)

  (def patch-from-read
    (nippy/thaw-from-file "patch.nippy"))

  (= patch patch-from-read)

  (require '[clojure.edn :as edn])

  (def big-tree
    (with-open [pbr (java.io.PushbackReader. (io/reader (io/file "big-tree.edn")))]
      (edn/read pbr)))

  (def big-tree-ref
    (convergent/ref big-tree))

  (nippy/freeze-to-file "big-tree.nippy" big-tree)

  (.length (io/file "big-tree.nippy"))

  (nippy/freeze-to-file "big-tree-ref.nippy" big-tree-ref)

  (.length (io/file "big-tree-ref.nippy"))

  ;;
  )
