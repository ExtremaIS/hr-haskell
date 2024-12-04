# `hr-haskell` `0.7.0.0` Release Notes

Date
: 2024-12-04

## Overview

`hr` is a utility for displaying a horizontal rule in a terminal.  It is
useful for marking a position in your terminal so that you can easily find it
again.  For example, use hr to display a horizontal rule before each build of
a project so that you can easily find the beginning of the output of the last
build.

A Haskell library is available, using package name [`horizontal-rule`][].

[`horizontal-rule`]: <https://hackage.haskell.org/package/horizontal-rule>

See the [README][] for details.

[README]: <https://github.com/ExtremaIS/hr-haskell#readme>

## This Release

This release adds compatibility with the latest releases of GHC and removes
support for versions of GHC that were released more than five years ago.  To
do so, dependencies `HMock` and `explainable-predicates` are vendored.

GHC versions 8.8.4 through 9.10.1 are supported.  Cabal version 3.0 through
3.12.1.0 are supported.

There are no changes to the API or CLI.

### Compatibility

To use this release with a Stackage snapshot that does not include it, add
the following to your `stack.yaml` configuration:

```yaml
extra-deps:
  - horizontal-rule-0.7.0.0
```

### Issues

There are no known issues at this time.
