# converge/transit

transit-(clj|cljs) handlers for Converge

## Usage

### Clojure + transit-clj

This is the transit-clj Usage example, modified to include a
ConvergentRef and the read/write handlers provided by this library.

``` clojure
(require '[cognitect.transit :as transit]
         '[converge.transit :as converge-transit]
         '[converge.api :as convergent])
(import [java.io ByteArrayInputStream ByteArrayOutputStream])

;; Write data to a stream
(def out (ByteArrayOutputStream. 4096))
(def writer
  (transit/writer out :json
                  {:handlers (merge transit/default-write-handlers
                                    converge-transit/write-handlers)}))
(def cref (convergent/ref {:foo :bar
                           :a   [1 2]}))
(transit/write writer cref)

cref

;; Take a peek at the JSON
(.toString out)
;; => "[\"~#opset/ref\", <snip a bunch of operations ...>]"

;; Read data from a stream
(def in (ByteArrayInputStream. (.toByteArray out)))
(def reader
  (transit/reader in :json
                  {:handlers (merge transit/default-read-handlers
                                    converge-transit/read-handlers)}))
(def read-ref (transit/read reader))
(prn read-ref)
;; => #converge.opset.ref.OpsetConvergentRef[{:status :ready, :val {:a [1 2], :foo :bar}} 0x3bd98fc8]
(prn @read-ref)
;; => {:a [1 2], :foo :bar}
```

### ClojureScript + transit-cljs

This is the transit-cljs Usage example, modified to include a
ConvergentRef and the read/write handlers provided by this library.



## Development

Invoke a library API function from the command-line:

    $ clojure -X converge.transit/foo :a 1 :b '"two"'
    {:a 1, :b "two"} "Hello, World!"

Run the project's tests (they'll fail until you edit them):

    $ clojure -T:build test

Run the project's CI pipeline and build a JAR (this will fail until you edit the tests to pass):

    $ clojure -T:build ci

This will produce an updated `pom.xml` file with synchronized dependencies inside the `META-INF`
directory inside `target/classes` and the JAR in `target`. You can update the version (and SCM tag)
information in generated `pom.xml` by updating `build.clj`.

Install it locally (requires the `ci` task be run first):

    $ clojure -T:build install

Deploy it to Clojars -- needs `CLOJARS_USERNAME` and `CLOJARS_PASSWORD` environment
variables (requires the `ci` task be run first):

    $ clojure -T:build deploy

Your library will be deployed to net.clojars.converge/transit on clojars.org by default.

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
