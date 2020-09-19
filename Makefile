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

# Clean

.PHONY: clean
clean:
	rm -rf target

# Development Workflow

.PHONY: dev
dev:
	clojure -A:dev${CLJ_REPL_ALIAS}

.PHONY: test
test:
	clojure -A:test:runner

# Project info

.PHONY: loc
loc:
	tokei dev/ resources/ src/ test/ Makefile *.edn
