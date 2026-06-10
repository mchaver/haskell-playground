# Developer tasks for itch-codec. Lint/format configs are shared and live at
# the repo root (../.hlint.yaml, ../fourmolu.yaml); both tools discover them by
# searching up from each source file.
#
# Tooling is expected on PATH; override per-invocation if needed, e.g.
#   make lint HLINT=~/.cabal/bin/hlint
CABAL    ?= cabal
HLINT    ?= hlint
FOURMOLU ?= fourmolu

HS_FILES := $(shell find src app test -name '*.hs')

.DEFAULT_GOAL := help

.PHONY: help
help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
		| awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-14s\033[0m %s\n", $$1, $$2}'

.PHONY: build
build: ## Compile the library, demo, and test suite
	$(CABAL) build all

.PHONY: test
test: ## Run the Hedgehog property suite
	$(CABAL) test --test-show-details=direct

.PHONY: run
run: ## Run the encode/decode demo
	$(CABAL) run -v0 itch-codec-demo

.PHONY: repl
repl: ## Open a GHCi session on the library
	$(CABAL) repl lib:itch-codec

.PHONY: lint
lint: ## Run HLint (shared house rules in ../.hlint.yaml)
	$(HLINT) src app test

.PHONY: format
format: ## Format all sources in place (../fourmolu.yaml)
	$(FOURMOLU) --mode inplace $(HS_FILES)

.PHONY: format-check
format-check: ## Fail if any source is not formatted (no writes)
	$(FOURMOLU) --mode check $(HS_FILES)

.PHONY: check
check: build test lint format-check ## Everything CI runs: build + test + lint + format-check

.PHONY: clean
clean: ## Remove build artifacts
	$(CABAL) clean
