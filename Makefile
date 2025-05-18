SOURCES = $(wildcard test/*.buscal)
SHELL = /usr/bin/env bash

.PHONY: test testall
testall:
	for v in $(SOURCES); do $(MAKE) test TARGET="$$v"; done

test:
	diff $(subst .buscal,.out,$(TARGET)) <(bash -c "$$(RUSTFLAGS=-Awarnings cargo run <${TARGET})")
