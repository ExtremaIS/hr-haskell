# `hr-haskell` `0.5.0.0` Release Notes

Date
: 2022-03-01

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

This is a major release.  The command-line utility interface has no changes,
but almost every aspect of the library API is changed.

### Package Name Change

The package is renamed to [`horizontal-rule`][] so that it can be uploaded to
Hackage.

### `MonadTerminal` and Mock Tests

The library API is refactored to use a type class (`MonadTerminal`) instead of
`IO` directory.  This allows much more of the implementation to be tested,
using the [HMock][] library.

[HMock]: <https://hackage.haskell.org/package/HMock>

### Dependency Versions

The following dependency version upper bounds have been bumped to support the
latest versions.

* [`optparse-applicative`](https://hackage.haskell.org/package/optparse-applicative)
* [`time`](https://hackage.haskell.org/package/time)
* [`text`](https://hackage.haskell.org/package/text)

### Compatibility

`hr` is currently tested with [GHC 8.2.2][] through [GHC 9.2.1][].  The
`.cabal` file uses Cabal version 1.24 (included with GHC 8.2.2), so it should
build fine on relatively old Haskell installations as well as current
installations.

To use this release with a Stackage snapshot that does not include it, add
the following to your `stack.yaml` configuration:

```yaml
extra-deps:
  - horizontal-rule-0.5.0.0
```

[GHC 8.2.2]: <https://www.haskell.org/ghc/download_ghc_8_2_2.html>
[GHC 9.2.1]: <https://www.haskell.org/ghc/download_ghc_9_2_1.html>

### Issues

There are no known issues at this time.
