# `hr-haskell` `0.6.0.0` Release Notes

Date
: 2023-05-28

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

This release adds compatibility with the latest version of the
`optparse-applicative` library.  Both lower and upper bounds of dependencies
are now tested in CI.  This release also includes changes to the project
management infrastructure.

There are no changes to the API or CLI.

### Compatibility

Build software:

| Software          | `hr` 0.5.0.0  | `hr` 0.6.0.0  |
| ----------------- | ------------- | ------------- |
| [GHC][]           | 8.2.2 ~ 9.2.1 | 8.2.2 ~ 9.6.2 |
| [cabal-install][] | 1.24 ~ 3.4    | 1.24 ~ 3.10   |

Library dependencies:

| Package           | `hr` 0.5.0.0      | `hr` 0.6.0.0        |
| ----------------- | ----------------- | ------------------- |
| [base][]          | `>=4.7 && <5`     | `>=4.10.1 && <4.19` |
| [terminal-size][] | `>=0.3 && <0.4`   | `>=0.2 && <0.4`     |
| [text][]          | `>=1.2.3 && <2.1` | `>=1.2.3 && <2.1`   |

Executable dependencies:

| Package                  | `hr` 0.5.0.0      | `hr` 0.6.0.0         |
| ------------------------ | ----------------- | -------------------- |
| [ansi-wl-pprint][]       | `>=0.6 && <0.7`   | `>=0.6.8 && <1.1`    |
| [optparse-applicative][] | `>=0.14 && <0.18` | `>=0.13 && <0.19`    |
| [prettyprinter][]        |                   | `>=1.7.1 && <1.8`    |
| [time][]                 | `>=1.8 && <1.13`  | `>=1.8.0.2 && <1.13` |

Test dependencies:

| Package         | `hr` 0.5.0.0      | `hr` 0.6.0.0     |
| --------------- | ----------------- | ---------------- |
| [tasty][]       | `>=1.0 && <1.5`   | `>=0.12 && <1.5` |
| [tasty-hunit][] | `>=0.10 && <0.11` | `>=0.8 && <0.11` |

To use this release with a Stackage snapshot that does not include it, add
the following to your `stack.yaml` configuration:

```yaml
extra-deps:
  - horizontal-rule-0.6.0.0
```

[GHC]: <https://www.haskell.org/ghc/>
[cabal-install]: <https://hackage.haskell.org/package/cabal-install>
[base]: <https://hackage.haskell.org/package/base>
[terminal-size]: <https://hackage.haskell.org/package/terminal-size>
[text]: <https://hackage.haskell.org/package/text>
[ansi-wl-pprint]: <https://hackage.haskell.org/package/ansi-wl-pprint>
[optparse-applicative]: <https://hackage.haskell.org/package/optparse-applicative>
[prettyprinter]: <https://hackage.haskell.org/package/prettyprinter>
[time]: <https://hackage.haskell.org/package/time>
[tasty]: <https://hackage.haskell.org/package/tasty>
[tasty-hunit]: <https://hackage.haskell.org/package/tasty-hunit>

### Issues

There are no known issues at this time.
