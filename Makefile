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

default: test

# Dependencies

.PHONY: bootstrap
bootstrap:
	-brew install clojure tokei
	wget "https://github.com/lambdaisland/funnel/releases/download/v0.1.42/funnel.darwin-amd64" -O bin/funnel
	chmod +x bin/funnel

.PHONY: outdated
outdated:
	clojure -R:cljs:dev:test -A:outdated

# Clean

.PHONY: clean
clean:
	rm -rf target

# Development Workflow

CLJ_REPL_ALIAS:=

.PHONY: clj-dev
clj-dev:
	clojure -A:dev:test${CLJ_REPL_ALIAS}

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
	yarn shadow-cljs -A:dev:test watch lib

# Tests

.PHONY: clj-test
clj-test: clean
	bin/kaocha unit-clj

.PHONY: cljs-test-harness
cljs-test-harness: shadow-start
	bin/funnel

.PHONY: cljs-test
cljs-test: clean
	yarn shadow-cljs -A:test release test
	open "http://localhost:8008"
	bin/kaocha unit-cljs

.PHONY: test
test: cljs-test clj-test

.PHONY: cljs-ci
cljs-ci: clean
	yarn shadow-cljs -A:test compile ci
	yarn karma start --single-run

.PHONY: ci
ci: clj-test cljs-ci

# Project info

.PHONY: loc
loc:
	tokei dev/ resources/ src/ test/ Makefile *.edn
