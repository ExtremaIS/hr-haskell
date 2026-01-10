# `hr-haskell` `0.7.1.0` Release Notes

Date
: 2026-01-10

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

This is a maintenance release that adds compatibility with GHC 9.14.1.  Mock
tests are removed because `HMock` is not actively maintained.

There are no changes to the API or CLI.

### Compatibility

GHC versions 8.8.4 through 9.14.1 are supported.  Note that GHC 9.12.3 is not
tested, however, because it has critical issues and is not available using
GHCup.

Cabal version 3.0 through 3.16.1.0 are supported.

To use this release with a Stackage snapshot that does not include it, add
the following to your `stack.yaml` configuration:

```yaml
extra-deps:
  - horizontal-rule-0.7.1.0
```

### Issues

There are no known issues at this time.
