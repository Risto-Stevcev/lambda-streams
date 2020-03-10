all: build

.PHONY: clean
clean:
	dune clean

.PHONY: build
build:
	dune build @all

.PHONY: fmt
fmt:
	dune build @fmt --auto-promote

.PHONY: docs
docs:
	dune build @doc

.PHONY: copy-docs
copy-docs: docs
	cp -r _build/default/_doc/_html/** docs/

.PHONY: open-docs
open-docs: copy-docs
	xdg-open docs/index.html

.PHONY: clean-docs
clean-docs:
	rm -rf docs/**

.PHONY: test
test:
	dune runtest --no-buffer

.PHONY: watch-test
watch-test:
	dune runtest --no-buffer -w
