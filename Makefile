##############################################################################
# Project configuration

PACKAGE    := hr
CABAL_FILE := $(PACKAGE).cabal
PROJECT    := $(PACKAGE)-haskell

MAINTAINER_NAME  = Travis Cardwell
MAINTAINER_EMAIL = travis.cardwell@extrema.is

DESTDIR     ?=
PREFIX      ?= /usr/local
bindir      ?= $(DESTDIR)/$(PREFIX)/bin
datarootdir ?= $(DESTDIR)/$(PREFIX)/share
docdir      ?= $(datarootdir)/doc/$(PROJECT)
man1dir     ?= $(datarootdir)/man/man1

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

.DEFAULT_GOAL := build

ifneq ($(origin CABAL), undefined)
  MODE := cabal
  CABAL_ARGS :=
  ifneq ($(origin PROJECT_FILE), undefined)
    CABAL_ARGS += "--project-file=$(PROJECT_FILE)"
  else
    PROJECT_FILE := cabal-$(shell ghc --version | sed 's/.* //').project
    ifneq (,$(wildcard $(PROJECT_FILE)))
      CABAL_ARGS += "--project-file=$(PROJECT_FILE)"
    endif
  endif
else
  MODE := stack
  STACK_ARGS :=
  ifneq ($(origin CONFIG), undefined)
    STACK_ARGS += --stack-yaml "$(CONFIG)"
  endif
  ifneq ($(origin RESOLVER), undefined)
    STACK_ARGS += --resolver "$(RESOLVER)"
  endif
  ifneq ($(origin STACK_NIX_PATH), undefined)
    STACK_ARGS += "--nix-path=$(STACK_NIX_PATH)"
  endif
endif

##############################################################################
# Functions

define all_files
  find . -not -path '*/\.*' -type f
endef

define checksum_files
  find . -maxdepth 1 -type f -not -path './*SUMS' | sed 's,^\./,,' | sort
endef

define die
  (echo "error: $(1)" ; false)
endef

define hs_files
  find . -not -path '*/\.*' -type f -name '*.hs'
endef

##############################################################################
# Rules

build: hr
build: # build package *
ifeq ($(MODE), cabal)
> cabal v2-build $(CABAL_ARGS)
else
> stack build $(STACK_ARGS)
endif
.PHONY: build

checksums: # calculate checksums of build artifacts
> @cd build && $(call checksum_files) | xargs md5sum > MD5SUMS
> @cd build && $(call checksum_files) | xargs sha1sum > SHA1SUMS
> @cd build && $(call checksum_files) | xargs sha256sum > SHA256SUMS
> @cd build && $(call checksum_files) | xargs sha512sum > SHA512SUMS
.PHONY: checksums

clean: # clean package
ifeq ($(MODE), cabal)
> @rm -rf dist-newstyle example/dist-newstyle
else
> @stack clean
> @cd example && stack clean
endif
.PHONY: clean

clean-all: clean
clean-all: # clean package and remove artifacts
> @rm -rf .hie
> @rm -rf .stack-work example/.stack-work
> @rm -rf build
> @rm -rf dist-newstyle example/dist-newstyle
> @rm -f *.yaml.lock example/*.yaml.lock
> @rm -f cabal.project.local example/cabal.project.local
> @rm -f result* example/result*
.PHONY: clean-all

deb: # build .deb package for VERSION in a Debian container
> $(eval VERSION := $(shell \
    grep '^version:' $(CABAL_FILE) | sed 's/^version: *//'))
> $(eval SRC := "$(PROJECT)-$(VERSION).tar.xz")
> @test -f build/$(SRC) || $(call die,"build/$(SRC) not found")
> @docker run --rm -it \
>   -e DEBFULLNAME="$(MAINTAINER_NAME)" \
>   -e DEBEMAIL="$(MAINTAINER_EMAIL)" \
>   -v $(PWD)/build:/host \
>   extremais/pkg-debian-stack:bullseye \
>   /home/docker/bin/make-deb.sh "$(SRC)"
.PHONY: deb

doc-api: hr
doc-api: # build API documentation *
ifeq ($(MODE), cabal)
> cabal v2-haddock $(CABAL_ARGS)
else
> stack haddock $(STACK_ARGS)
endif
.PHONY: doc-api

grep: # grep all non-hidden files for expression E
> $(eval E:= "")
> @test -n "$(E)" || $(call die,"usage: make grep E=expression")
> @$(call all_files) | xargs grep -Hn '$(E)' || true
.PHONY: grep

help: # show this help
> @grep '^[a-zA-Z0-9_-]\+:[^#]*# ' $(MAKEFILE_LIST) \
>   | sed 's/^\([^:]\+\):[^#]*# \(.*\)/make \1\t\2/' \
>   | column -t -s $$'\t'
> @echo
> @echo "* Set CABAL to use Cabal instead of Stack."
> @echo "* Set CONFIG to specify a Stack configuration file."
> @echo "* Set PROJECT_FILE to specify a cabal.project file."
> @echo "* Set RESOLVER to specify a Stack resolver."
> @echo "* Set STACK_NIX_PATH to specify a Stack Nix path."
.PHONY: help

hlint: # run hlint on all Haskell source
> @$(call hs_files) | xargs hlint
.PHONY: hlint

hr: #internal# display a horizontal rule
> @command -v hr >/dev/null 2>&1 && hr -t || true
.PHONY: hr

hsgrep: # grep all Haskell source for expression E
> $(eval E := "")
> @test -n "$(E)" || $(call die,"usage: make hsgrep E=expression")
> @$(call hs_files) | xargs grep -Hn '$(E)' || true
.PHONY: hsgrep

hsrecent: # show N most recently modified Haskell files
> $(eval N := "10")
> @find . -not -path '*/\.*' -type f -name '*.hs' -printf '%T+ %p\n' \
>   | sort --reverse \
>   | head -n $(N)
.PHONY: hsrecent

hssloc: # count lines of Haskell source
> @$(call hs_files) | xargs wc -l | tail -n 1 | sed 's/^ *\([0-9]*\).*$$/\1/'
.PHONY: hssloc

install: install-bin
install: install-man
install: install-doc
install: # install everything to PREFIX (*)
.PHONY: install

install-bin: build
install-bin: # install executable to PREFIX/bin (*)
> @mkdir -p "$(bindir)"
ifeq ($(MODE), cabal)
> @install -m 0755 \
>   "$(shell cabal list-bin $(CABAL_ARGS) hr)" \
>   "$(bindir)/hr"
else
> $(eval LIROOT := $(shell stack path --local-install-root))
> @install -m 0755 "$(LIROOT)/bin/hr" "$(bindir)/hr"
endif
.PHONY: install-bin

install-doc: # install documentation to PREFIX/share/doc/hr-haskell
> @mkdir -p "$(docdir)"
> @install -m 0644 -T <(gzip -c README.md) "$(docdir)/README.md.gz"
> @install -m 0644 -T <(gzip -c CHANGELOG.md) "$(docdir)/changelog.gz"
> @install -m 0644 -T <(gzip -c LICENSE) "$(docdir)/LICENSE.gz"
.PHONY: install-doc

install-man: # install manual to PREFIX/share/man/man1
> @mkdir -p "$(man1dir)"
> @install -m 0644 -T <(gzip -c doc/hr.1) "$(man1dir)/hr.1.gz"
.PHONY: install-man

man: # build man page
> $(eval VERSION := $(shell \
    grep '^version:' $(CABAL_FILE) | sed 's/^version: *//'))
> $(eval DATE := $(shell date --rfc-3339=date))
> @pandoc -s -t man -o doc/hr.1 \
>   --variable header="hr Manual" \
>   --variable footer="$(PROJECT) $(VERSION) ($(DATE))" \
>   doc/hr.1.md
.PHONY: man

recent: # show N most recently modified files
> $(eval N := "10")
> @find . -not -path '*/\.*' -type f -printf '%T+ %p\n' \
>   | sort --reverse \
>   | head -n $(N)
.PHONY: recent

repl: # enter a REPL *
ifeq ($(MODE), cabal)
> cabal repl $(CABAL_ARGS)
else
> stack exec ghci $(STACK_ARGS)
endif
.PHONY: repl

rpm: # build .rpm package for VERSION in a Fedora container
> $(eval VERSION := $(shell \
    grep '^version:' $(CABAL_FILE) | sed 's/^version: *//'))
> $(eval SRC := "$(PROJECT)-$(VERSION).tar.xz")
> @test -f build/$(SRC) || $(call die,"build/$(SRC) not found")
> @docker run --rm -it \
>   -e RPMFULLNAME="$(MAINTAINER_NAME)" \
>   -e RPMEMAIL="$(MAINTAINER_EMAIL)" \
>   -v $(PWD)/build:/host \
>   extremais/pkg-fedora-stack:34 \
>   /home/docker/bin/make-rpm.sh "$(SRC)"
.PHONY: rpm

sdist: # create source tarball for Hackage
> $(eval BRANCH := $(shell git rev-parse --abbrev-ref HEAD))
> @test "${BRANCH}" = "main" || $(call die,"not in main branch")
ifeq ($(MODE), cabal)
> @cabal sdist
else
> @stack sdist
endif
.PHONY: sdist

source-git: # create source tarball of git TREE
> $(eval TREE := "HEAD")
> $(eval BRANCH := $(shell git rev-parse --abbrev-ref $(TREE)))
> @test "$(BRANCH)" = "main" || echo "WARNING: Not in main branch!" >&2
> $(eval DIRTY := $(shell git diff --shortstat | wc -l))
> @test "$(DIRTY)" = "0" \
>   || echo "WARNING: Not including non-committed changes!" >&2
> $(eval UNTRACKED := $(shell \
    git ls-files --other --directory --no-empty-directory --exclude-standard \
    | wc -l))
> @test "$(UNTRACKED)" = "0" \
>   || echo "WARNING: Not including untracked files!" >&2
> $(eval VERSION := $(shell \
    grep '^version:' $(CABAL_FILE) | sed 's/^version: *//'))
> @mkdir -p build
> @git archive --format=tar --prefix=$(PROJECT)-$(VERSION)/ $(TREE) \
>   | xz \
>   > build/$(PROJECT)-$(VERSION).tar.xz
.PHONY: source-git

source-tar: # create source tarball using tar
> $(eval DIRTY := $(shell git diff --shortstat | wc -l))
> @test "$(DIRTY)" = "0" \
>   || echo "WARNING: Including non-committed changes!" >&2
> $(eval UNTRACKED := $(shell \
    git ls-files --other --directory --no-empty-directory --exclude-standard \
    | wc -l))
> @test "$(UNTRACKED)" = "0" \
>   || echo "WARNING: Including untracked files!" >&2
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

stan: hr
stan: export STAN_USE_DEFAULT_CONFIG=True
stan: # run stan static analysis
ifeq ($(MODE), cabal)
> @cabal v2-build -f write-hie
else
> @stack build --flag $(PACKAGE):write-hie
endif
> @stan
.PHONY: stan

test: hr
test: # run tests, optionally for pattern P *
> $(eval P := "")
ifeq ($(MODE), cabal)
> @test -z "$(P)" \
>   && cabal v2-test --enable-tests --test-show-details=always \
>       $(CABAL_ARGS) \
>   || cabal v2-test --enable-tests --test-show-details=always \
>       --test-option '--patern=$(P)' $(CABAL_ARGS)
else
> @test -z "$(P)" \
>   && stack test $(STACK_ARGS) \
>   || stack test $(STACK_ARGS) --test-arguments '--pattern $(P)'
endif
.PHONY: test

test-all: # run tests for all configured Stackage releases
ifeq ($(MODE), cabal)
> $(call die,"test-all not supported in CABAL mode")
endif
> @command -v hr >/dev/null 2>&1 && hr "stack-8.2.2.yaml" || true
> @make test CONFIG=stack-8.2.2.yaml
> @command -v hr >/dev/null 2>&1 && hr "stack-8.4.4.yaml" || true
> @make test CONFIG=stack-8.4.4.yaml
> @command -v hr >/dev/null 2>&1 && hr "stack-8.6.5.yaml" || true
> @make test CONFIG=stack-8.6.5.yaml
> @command -v hr >/dev/null 2>&1 && hr "stack-8.8.4.yaml" || true
> @make test CONFIG=stack-8.8.4.yaml
> @command -v hr >/dev/null 2>&1 && hr "stack-8.10.7.yaml" || true
> @make test CONFIG=stack-8.10.7.yaml
> @command -v hr >/dev/null 2>&1 && hr "stack-9.0.1.yaml" || true
> @make test CONFIG=stack-9.0.1.yaml
> @command -v hr >/dev/null 2>&1 && hr "stack-9.2.1.yaml" || true
> @make test CONFIG=stack-9.2.1.yaml
.PHONY: test-all

test-nightly: # run tests for the latest Stackage nightly release
ifeq ($(MODE), cabal)
> $(call die,"test-nightly not supported in CABAL mode")
endif
> @make test RESOLVER=nightly
.PHONY: test-nightly

todo: # search for TODO items
> @find . -type f \
>   -not -path '*/\.*' \
>   -not -path './build/*' \
>   -not -path './project/*' \
>   -not -path ./Makefile \
>   | xargs grep -Hn TODO \
>   | grep -v '^Binary file ' \
>   || true
.PHONY: todo

version: # show current version
> @grep '^version:' $(CABAL_FILE) | sed 's/^version: */$(PROJECT) /'
.PHONY: version
