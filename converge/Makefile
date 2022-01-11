# Copyright 2020 Evident Systems LLC

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at

#     http://www.apache.org/licenses/LICENSE-2.0

# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

default: ci

# Dependencies

.PHONY: bootstrap
bootstrap:
	-brew install clojure tokei

.PHONY: outdated
outdated:
	clojure -M:cljs:dev:test:outdated

# Clean

.PHONY: clean
clean:
	rm -rf target

# Development Workflow

CLJ_REPL_ALIAS:=

.PHONY: clj-dev
clj-dev:
	clojure -M:dev:test${CLJ_REPL_ALIAS}

node_modules/.yarn-integrity: yarn.lock package.json
	yarn install

.PHONY: shadow-start
shadow-start:
	yarn shadow-cljs -A:dev:test start

.PHONY: shadow-stop
shadow-stop:
	yarn shadow-cljs -A:dev:test stop

.PHONY: cljs-dev
cljs-dev: node_modules/.yarn-integrity shadow-start
	yarn shadow-cljs -A:dev:test${CLJ_REPL_ALIAS} watch lib

# Tests

.PHONY: clj-test
clj-test: clean
	clojure -T:build test :aliases [:clj]

.PHONY: cljs-test
cljs-test: clean
	yarn shadow-cljs -A:test compile ci
	yarn karma start --single-run

.PHONY: test
test: clj-test cljs-test

.PHONY: clj-ci
clj-ci: clj-test

.PHONY: cljs-ci
cljs-ci: cljs-test

.PHONY: ci
ci: clj-ci cljs-ci

BENCH_EDN_FILE:=../big-tree.edn

.PHONY: clj-bench
clj-bench:
	clojure -M:bench -m converge.bench $(BENCH_EDN_FILE)

target/bench/js/bench.js: deps.edn shadow-cljs.edn node_modules/.yarn-integrity src/
	yarn shadow-cljs -A:bench release bench

.PHONY: cljs-bench
cljs-bench: target/bench/js/bench.js
	node target/bench/js/bench.js $(BENCH_EDN_FILE)

.PHONY: bench
bench: clj-bench cljs-bench

target/dist/js/converge.js: deps.edn shadow-cljs.edn node_modules/.yarn-integrity src/
	yarn shadow-cljs release lib

.PHONY: build
build: target/dist/js/converge.js

# Project info

.PHONY: loc
loc:
	tokei dev/ resources/ src/ test/ Makefile *.edn
