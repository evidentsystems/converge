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
	-brew install tokei

# Tests

.PHONY: test
test:
	$(MAKE) -C nippy test
	$(MAKE) -C transit test
	$(MAKE) -C storage test
	$(MAKE) -C converge test

.PHONY: ci
ci:
	$(MAKE) -C nippy ci
	$(MAKE) -C transit ci
	$(MAKE) -C storage ci
	$(MAKE) -C converge ci

# Build

.PHONY: clean
clean:
	$(MAKE) -C nippy clean
	$(MAKE) -C transit clean
	$(MAKE) -C storage clean
	$(MAKE) -C converge clean

# Project info

.PHONY: loc
loc:
	$(MAKE) -C nippy loc
	$(MAKE) -C transit loc
	$(MAKE) -C storage loc
	$(MAKE) -C converge loc
