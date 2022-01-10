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

# Clean

.PHONY: clean
clean:
	rm -rf target

# Development Workflow

CLJ_REPL_ALIAS:=

.PHONY: clj-dev
clj-dev:
	clojure -M:local:clj:dev:test${CLJ_REPL_ALIAS}

node_modules/.yarn-integrity: yarn.lock package.json
	yarn install

.PHONY: shadow-start
shadow-start:
	yarn shadow-cljs -A:local:dev:test start

.PHONY: shadow-stop
shadow-stop:
	yarn shadow-cljs -A:local:dev:test stop

.PHONY: cljs-dev
cljs-dev: node_modules/.yarn-integrity shadow-start
	yarn shadow-cljs -A:local:dev:test${CLJ_REPL_ALIAS} watch lib

# Tests

.PHONY: clj-test
clj-test:
	clojure -T:local:build test :aliases [:clj]

.PHONY: cljs-test
cljs-test: clean
	yarn shadow-cljs -A:local:test compile ci
	yarn karma start --single-run

.PHONY: test
test: clj-test cljs-test

.PHONY: clj-ci
clj-ci:
	clojure -T:build test :aliases [:clj]

.PHONY: cljs-ci
cljs-ci: clean
	yarn shadow-cljs -A:test compile ci
	yarn karma start --single-run

.PHONY: ci
ci: clj-ci cljs-ci

target/dist/js/converge.js: deps.edn shadow-cljs.edn node_modules/.yarn-integrity src/
	yarn shadow-cljs -A:test release lib

.PHONY: build
build: target/dist/js/converge.js

# Project info

.PHONY: loc
loc:
	tokei dev/ src/ test/ Makefile *.edn
