.PHONY: help deps test format lint dialyzer docs clean quality coverage

help: ## Show this help
	@echo "Available targets:"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'

deps: ## Install dependencies
	mix deps.get

test: ## Run tests
	mix test

format: ## Format code
	mix format

format-check: ## Check code formatting
	mix format --check-formatted

lint: ## Run Credo linter
	mix credo --strict

dialyzer: ## Run Dialyzer
	mix dialyzer

docs: ## Generate documentation
	mix docs

clean: ## Clean build artifacts
	mix clean
	rm -rf _build deps doc priv/plts

quality: format-check lint ## Run all quality checks
	mix compile --warnings-as-errors

coverage: ## Run tests with coverage
	mix coveralls.html

security: ## Run security analysis
	mix sobelow --skip

all: quality test dialyzer ## Run all checks and tests

setup: deps ## Initial project setup
	mix compile
	mix dialyzer --plt
	mix git_hooks.install