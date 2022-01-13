# converge/storage

An API for persisting convergent refs to the filesystem in a Git-friendly way.

## Usage

Convergent refs are stored on disk as a directory of Nippy serialized
patch files, which together comprise the OpSet of the ref.

``` clojure
(def dirname "a-ref-dir")

(def r (convergent/ref {:foo :bar :baz :quux}))

(sync-directory r dirname)
;=> returns the name of the patch file added to the directory to bring the
;   on-disk ref to (at least) the same clock point as the in-memory ref, or
;   nil if no file was needed/written. If directory doesn't contain a ref,
;   creates the root file and returns its name. Patch filenames are SHA256-3
;   hashes of their contents, so writing patches is idempotent.

(def r (from-directory dirname))
;=> returns a ConvergentRef having the same clock state as on-disk

@r

(swap! r assoc :quux [1 2 :lalala #{:a 'nice "set"} {:of [2 'things]}])

(sync-directory r dirname)
```

## License

Copyright 2020 Evident Systems LLC

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
