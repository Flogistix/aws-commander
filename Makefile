.PHONY: watch
watch:
	stack build --file-watch

.PHONY: build
build:
	stack build

.PHONY: docs
docs:
	stack build --haddock --open --no-haddock-deps

.PHONY: clean
clean:
	stack clean
