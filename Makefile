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
	brew install clojure tokei

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
	clojure -A:dev${CLJ_REPL_ALIAS}

node_modules/.yarn-integrity: yarn.lock package.json
	yarn install

.PHONY: cljs-dev
cljs-dev: node_modules/.yarn-integrity
	yarn shadow-cljs -A:dev watch lib

# Tests

.PHONY: clj-test
clj-test:
	clojure -A:test:runner

.PHONY: cljs-test
cljs-test: clean
	yarn shadow-cljs -A:test compile ci
	yarn karma start --single-run

.PHONY: test
test: clj-test cljs-test

# Project info

.PHONY: loc
loc:
	tokei dev/ resources/ src/ test/ Makefile *.edn
