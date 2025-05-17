SOURCES = $(wildcard test/*.ruscal)
EXPECTS = (SOURCES:%.ruscal=%.out)
SHELL = /usr/bin/env bash

.PHONY: test
test2:
	for file in "$(SOURCES)" ; do \
		# bash -c "$$(cargo run <$$file)"; \
	done

all:
	for v in $(SOURCES); do $(MAKE) test TARGET="$$v"; done

test:
	diff $(subst .ruscal,.out,$(TARGET)) <(bash -c "$$(RUSTFLAGS=-Awarnings cargo run <${TARGET})")
