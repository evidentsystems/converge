# converge/nippy

Nippy serialization support for Converge in Clojure.

## Usage

Nippy provides a global extension mechanism for supporting custom
types. Simply requiring the `converge.nippy` namespace adds support
for Converge types:

``` clojure
(require '[taoensso.nippy :as nippy]
         'converge.nippy
         '[converge.api :as convergent])

(def r (convergent/ref {:foo [:bar #{:baz}]}))

(def nippy-bytes (nippy/freeze r))

(def roundtripped (nippy/thaw nippy-bytes))

@roundtripped
;=> {:foo [:bar #{:baz}]}

(def r2 (convergent/ref-from-ops (convergent/ref-log r)))

(swap! r assoc :baz :quux)

(def patch (convergent/patch-from-clock r (convergent/clock r2)))

(nippy/thaw (nippy/freeze patch))
;=> <the patch>

(def clock (convergent/clock r1))
(nippy/thaw (nippy/freeze clock))
;=> <the clock>
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
