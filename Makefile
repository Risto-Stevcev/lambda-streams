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

.PHONY: doc
doc:
	dune build @doc

.PHONY: open-doc
open-doc:
	xdg-open _build/default/_doc/_html/index.html

.PHONY: test
test:
	dune runtest --no-buffer

.PHONY: watch-test
watch-test:
	dune runtest --no-buffer -w
