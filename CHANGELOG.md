# `hr-haskell` Changelog

This project follows the [Haskell package versioning policy][PVP], with
versions in `A.B.C.D` format.  `A` may be incremented arbitrarily for
non-technical reasons, but [semantic versioning][SemVer] is otherwise
followed, where `A.B` is the major version, `C` is the minor version, and `D`
is the patch version.  Initial development uses versions `0.0.0.D`, for which
every version is considered breaking.

[PVP]: <https://pvp.haskell.org/>
[SemVer]: <https://semver.org/>

The format of this changelog is based on [Keep a Changelog][KaC], with the
following conventions:

* Level-two heading `Unreleased` is used to track changes that have not been
  released.
* Other level-two headings specify the release in `A.B.C.D (YYYY-MM-DD)`
  format, with newer versions above older versions.
* Level-three headings are used to categorize changes as follows:
    1. Breaking
    2. Non-Breaking
* Changes are listed in arbitrary order and present tense.

[KaC]: <https://keepachangelog.com/en/1.0.0/>

## Unreleased

### Non-Breaking

* Add Cabal support to `Makefile`

## 0.1.0.3 (2020-11-21)

### Non-Breaking

* Use GitHub Actions instead of Travis CI

## 0.1.0.2 (2020-11-05)

### Non-Breaking

* Rename Git default branch to `main`
* Refactor `Makefile`, add `STACK_NIX_PATH` support
* Add `test-all` command to `Makefile`
* Add Nix configuration

## 0.1.0.1 (2019-12-29)

### Non-Breaking

* Fix operator for compatibility with GHC 8.2.2

## 0.1.0.0 (2019-12-29)

### Breaking

* Initial public release
