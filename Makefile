##############################################################################
# Project configuration

PACKAGE    := hr
BINARY     := $(PACKAGE)
CABAL_FILE := $(PACKAGE).cabal
PROJECT    := $(PACKAGE)-haskell

##############################################################################
# Make configuration

ifeq ($(origin .RECIPEPREFIX), undefined)
  $(error GNU Make 4.0 or later required)
endif
.RECIPEPREFIX := >

SHELL := bash
.SHELLFLAGS := -o nounset -o errexit -o pipefail -c

MAKEFLAGS += --no-builtin-rules
MAKEFLAGS += --warn-undefined-variables

NIX_PATH_ARGS :=
ifneq ($(origin STACK_NIX_PATH), undefined)
  NIX_PATH_ARGS := "--nix-path=$(STACK_NIX_PATH)"
endif

RESOLVER_ARGS :=
ifneq ($(origin RESOLVER), undefined)
  RESOLVER_ARGS := "--resolver" "$(RESOLVER)"
endif

STACK_YAML_ARGS :=
ifneq ($(origin CONFIG), undefined)
  STACK_YAML_ARGS := "--stack-yaml" "$(CONFIG)"
endif

##############################################################################
# Functions

define all_files
  find . -not -path '*/\.*' -type f
endef

define die
  (echo "error: $(1)" ; false)
endef

define hs_files
  find . -not -path '*/\.*' -type f -name '*.hs'
endef

##############################################################################
# Rules

_default: build
.PHONY: _default

build:
> @command -v hr >/dev/null 2>&1 && hr -t || true
> @stack build $(RESOLVER_ARGS) $(STACK_YAML_ARGS) $(NIX_PATH_ARGS)
.PHONY: build

clean:
> @stack clean
.PHONY: clean

clean-all: clean
> @rm -rf .stack-work
> @rm -rf build
> @rm -f *.yaml.lock
.PHONY: clean-all

grep:
> $(eval E:= "")
> @test -n "$(E)" || $(call die,"usage: make grep E=expression")
> @$(call all_files) | xargs grep -Hn '$(E)' || true
.PHONY: grep

help:
> @echo "make build       build package (default) *"
> @echo "make clean       clean package"
> @echo "make clean-all   clean package and remove artifacts"
> @echo "make grep        grep all non-hidden files for expression E"
> @echo "make help        show this help"
> @echo "make hlint       run hlint on all Haskell source"
> @echo "make hsgrep      grep all Haskell source for expression E"
> @echo "make hsrecent    show N most recently modified Haskell files"
> @echo "make hssloc      count lines of Haskell source"
> @echo "make man         build man page"
> @echo "make recent      show N most recently modified files"
> @echo "make repl        enter a REPL *"
> @echo "make source-git  create source tarball of git TREE"
> @echo "make source-tar  create source tarball using tar"
> @echo "make test        run tests, optionally for pattern P *"
> @echo "make test-all    run tests for all versions"
> @echo "make todo        search for TODO items"
> @echo "make version     show current version"
> @echo
> @echo "* Use STACK_NIX_PATH to specify a Nix path."
> @echo "* Use RESOLVER to specify a resolver."
> @echo "* Use CONFIG to specify a Stack configuration file."
.PHONY: help

hlint:
> @$(call hs_files) | xargs hlint
.PHONY: hlint

hsgrep:
> $(eval E := "")
> @test -n "$(E)" || $(call die,"usage: make hsgrep E=expression")
> @$(call hs_files) | xargs grep -Hn '$(E)' || true
.PHONY: hsgrep

hsrecent:
> $(eval N := "10")
> @find . -not -path '*/\.*' -type f -name '*.hs' -printf '%T+ %p\n' \
>   | sort --reverse \
>   | head -n $(N)
.PHONY: hsrecent

hssloc:
> @$(call hs_files) | xargs wc -l | tail -n 1 | sed 's/^ *\([0-9]*\).*$$/\1/'
.PHONY: hssloc

man:
> $(eval VERSION := $(shell \
    grep '^version:' $(CABAL_FILE) | sed 's/^version: *//'))
> $(eval DATE := $(shell date --rfc-3339=date))
> @mkdir -p build
> @pandoc -s -t man -o build/$(BINARY).1 \
>   --variable header="$(BINARY) Manual" \
>   --variable footer="$(PROJECT) $(VERSION) ($(DATE))" \
>   doc/$(BINARY).1.md
.PHONY: man

recent:
> $(eval N := "10")
> @find . -not -path '*/\.*' -type f -printf '%T+ %p\n' \
>   | sort --reverse \
>   | head -n $(N)
.PHONY: recent

repl:
> @stack exec ghci $(RESOLVER_ARGS) $(STACK_YAML_ARGS) $(NIX_PATH_ARGS)
.PHONY: repl

source-git:
> $(eval TREE := "HEAD")
> $(eval BRANCH := $(shell git rev-parse --abbrev-ref $(TREE)))
> @test "${BRANCH}" = "master" || echo "WARNING: Not in master branch!" >&2
> $(eval VERSION := $(shell \
    grep '^version:' $(CABAL_FILE) | sed 's/^version: *//'))
> @mkdir -p build
> @git archive --format=tar --prefix=$(PROJECT)-$(VERSION)/ $(TREE) \
>   | xz \
>   > build/$(PROJECT)-$(VERSION).tar.xz
.PHONY: source-git

source-tar:
> $(eval VERSION := $(shell \
    grep '^version:' $(CABAL_FILE) | sed 's/^version: *//'))
> @mkdir -p build
> @sed -e 's,^/,./,' -e 's,/$$,,' .gitignore > build/.gitignore
> @tar \
>   --exclude-vcs \
>   --exclude-ignore-recursive=build/.gitignore \
>   --transform "s,^\.,$(PROJECT)-$(VERSION)," \
>   -Jcf build/$(PROJECT)-$(VERSION).tar.xz \
>   .
> @rm -f build/.gitignore
.PHONY: source-tar

test:
> $(eval P := "")
> @command -v hr >/dev/null 2>&1 && hr -t || true
> @test -z "$(P)" \
>   && stack test $(RESOLVER_ARGS) $(STACK_YAML_ARGS) $(NIX_PATH_ARGS) \
>   || stack test $(RESOLVER_ARGS) $(STACK_YAML_ARGS) $(NIX_PATH_ARGS) \
>       --test-arguments '--pattern $(P)'
.PHONY: test

test-all:
> $(eval CONFIG := $(shell \
    test -f stack-nix-8.2.2.yaml \
    && echo stack-nix-8.2.2.yaml \
    || echo stack-8.2.2.yaml))
> @command -v hr >/dev/null 2>&1 && hr $(CONFIG) || true
> @make test CONFIG=$(CONFIG)
> $(eval CONFIG := $(shell \
    test -f stack-nix-8.4.4.yaml \
    && echo stack-nix-8.4.4.yaml \
    || echo stack-8.4.4.yaml))
> @command -v hr >/dev/null 2>&1 && hr $(CONFIG) || true
> @make test CONFIG=$(CONFIG)
> $(eval CONFIG := $(shell \
    test -f stack-nix-8.6.5.yaml \
    && echo stack-nix-8.6.5.yaml \
    || echo stack-8.6.5.yaml))
> @command -v hr >/dev/null 2>&1 && hr $(CONFIG) || true
> @make test CONFIG=$(CONFIG)
> $(eval CONFIG := $(shell \
    test -f stack-nix.yaml \
    && echo stack-nix.yaml \
    || echo stack.yaml))
> @command -v hr >/dev/null 2>&1 && hr $(CONFIG) || true
> @make test CONFIG=$(CONFIG)
> $(eval STACK_NIX_PATH := $(shell \
    test -f stack-nix-nightly.path \
    && cat stack-nix-nightly.path \
    || true))
> @command -v hr >/dev/null 2>&1 && hr nightly || true
> @test -f stack-nix-nightly.path \
>   && make test RESOLVER=nightly STACK_NIX_PATH="$(STACK_NIX_PATH)" \
>   || make test RESOLVER=nightly
.PHONY: test-all

todo:
> @find . -type f \
>   -not -path '*/\.*' \
>   -not -path './build/*' \
>   -not -path './project/*' \
>   -not -path ./Makefile \
>   | xargs grep -Hn TODO || true
.PHONY: todo

version:
> @grep '^version:' $(CABAL_FILE) | sed 's/^version: */$(PROJECT) /'
.PHONY: version
