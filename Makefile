all: build

.PHONY: clean
clean:
	opam exec -- dune clean
	yarn bsb -clean-world

.PHONY: build
build:
	opam exec -- dune build @all
	yarn bsb -make-world

.PHONY: fmt
fmt:
	opam exec -- dune build @fmt --auto-promote

.PHONY: docs
docs: clean-docs
	opam exec -- dune build @doc

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
	opam exec -- dune runtest --no-buffer

.PHONY: watch
watch:
	opam exec -- dune build @all -w

.PHONY: watch-test
watch-test:
	opam exec -- dune runtest --no-buffer -w

.PHONY: watch-bs
watch-bs:
	yarn bsb -make-world -w

.PHONY: utop
utop:
	opam exec -- dune utop .

.PHONY: remove-switch
remove-switch:
	opam switch remove -y .

.PHONY: dev-tools
dev-tools:
	opam install -y merlin ocamlformat utop

.PHONY: create-4.08-switch
create-4.08-switch:
	opam switch create -y . 4.08.1 -t -d

4.08-switch: remove-switch create-4.08-switch dev-tools

.PHONY: default-switch
default-switch:
	opam switch create -y . -t -d
	make dev-tools
	eval $(opam env)
