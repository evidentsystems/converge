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

.PHONY: dev
dev:
	clojure -M:local:dev${CLJ_REPL_ALIAS}

# Tests

.PHONY: test
test:
	clojure -T:local:build test :aliases [:clj]

.PHONY: ci
ci:
	clojure -T:build test :aliases [:clj]

# Project info

.PHONY: loc
loc:
	tokei dev/ src/ test/ Makefile *.edn
