SOURCES = $(wildcard test/*.bscl)
SHELL = /usr/bin/env bash

.PHONY: test testall
testall:
	for v in $(SOURCES); do \
	  if $(MAKE) test TARGET="$$v"; then \
      success=$$((success + 1)); \
		else \
      fail=$$((fail+1)); \
		fi \
	done; \
	echo "success: $${success:-0} fail: $${fail:-0}";

test:
	diff $(subst .bscl,.out,$(TARGET)) <(RUSTFLAGS=-Awarnings cargo run run ${TARGET})
