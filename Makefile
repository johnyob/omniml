.DEFAULT_GOAL := all

ifneq ($(shell command -v opam 2>/dev/null),)
DUNE := opam exec -- dune
else
DUNE := dune
endif

.PHONY: all
all: build

.PHONY: install-ocamlformat
install-ocamlformat:
	opam install -y ocamlformat=0.26.2

.PHONY: install-deps
install-opam-deps: install-opam-switch install-ocamlformat
	opam install -y ocaml-lsp-server
	opam install -y --deps-only --with-test --with-doc .

.PHONY: install-opam-switch
install-opam-switch:
	opam switch create .

.PHONY: build
build:
	$(DUNE) build

.PHONY: install
install: all 
	$(DUNE) install --root .

.PHONY: test
test:
	$(DUNE) runtest

.PHONY: clean
clean:
	$(DUNE) clean

.PHONY: doc
doc:
	$(DUNE) build @doc

.PHONY: watch
watch:
	$(DUNE) build @run -w --force --no-buffer

.PHONY: utop
utop:
	$(DUNE) utop
