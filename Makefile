SHELL := /bin/sh

CWD := $(shell pwd)
SBCL ?= sbcl
QL ?= $(HOME)/quicklisp/setup.lisp

SBCL_FLAGS := --dynamic-space-size 8096 --noinform --disable-debugger --non-interactive
ASDF_BOOT := --eval '(require :asdf)' --eval "(pushnew \#p\"$(CWD)/\" asdf:*central-registry*)"
QL_BOOT := --eval '(load "$(QL)")'

.PHONY: all build test clean install

all: build

check-quicklisp:
	@test -f "$(QL)" || { \
	  echo "Quicklisp not found at $(QL). Override QL=/path/to/quicklisp/setup.lisp"; \
	  exit 1; \
	}

build: check-quicklisp
	$(SBCL) $(SBCL_FLAGS) $(QL_BOOT) $(ASDF_BOOT) \
	  --eval '(asdf:make "beadwork")'
	@mkdir -p bin
	@if [ -f bw ]; then mv -f bw bin/bw; fi
	@echo "Built binary: bin/bw"

test: check-quicklisp
	$(SBCL) $(SBCL_FLAGS) $(QL_BOOT) $(ASDF_BOOT) \
	  --eval '(asdf:test-system "beadwork")'

clean:
	@rm -f bw bin/bw
	@find . -type f \( -name "*.fasl" -o -name "*.x86f" -o -name "*.fas" \) -print0 | xargs -0 -r rm -f

install: build
	@mkdir -p $(HOME)/.local/bin
	@if [ -f bin/bw ]; then cp -f bin/bw $(HOME)/.local/bin/bw; fi
	@echo "Installed bw to ~/.local/bin/bw"
