
.PHONY: watch
watch:
	stack build --file-watch

.PHONY: build
build:
	stack build
