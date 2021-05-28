# hr

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![GitHub CI](https://github.com/ExtremaIS/hr-haskell/workflows/CI/badge.svg?branch=main)](https://github.com/ExtremaIS/hr-haskell/actions)

* [Overview](#overview)
* [CLI](#cli)
    * [Requirements](#requirements)
    * [Installation](#installation)
        * [Installation From Source](#installation-from-source)
        * [`.deb` Package Installation](#deb-package-installation)
        * [`.rpm` Package Installation](#rpm-package-installation)
    * [Usage](#usage)
        * [Examples](#examples)
* [Library](#library)
* [Project](#project)
    * [Links](#links)
    * [Releases](#releases)
    * [Contribution](#contribution)
    * [License](#license)

## Overview

`hr` is a utility for displaying a horizontal rule in a terminal.

It is useful for marking a position in your terminal so that you can easily
find it again.  For example, use `hr` to display a horizontal rule before each
build of a project so that you can easily find the beginning of the output of
the last build.

## CLI

### Requirements

`hr` has only been tested on Linux.  It *might* work on Windows and macOS.

### Installation

#### Installation From Source

`hr` can be built from source using [Stack](https://www.haskellstack.org).
For example, you can install the latest release (to `/usr/bin` on Linux) as
follows:

```
$ git clone https://github.com/ExtremaIS/hr-haskell.git
$ cd hr-haskell
$ make
$ sudo make install
```

##### `.deb` Package Installation

Check the [Releases][] page for `.deb` packages.

##### `.rpm` Package Installation

Check the [Releases][] page for `.rpm` packages.

[Releases]: <https://github.com/ExtremaIS/hr-haskell/releases>

### Usage

See the [`hr` man page](doc/hr.1.md) for usage information.

#### Examples

The rule fills with width of the terminal by default:

```
$ hr
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

In cases when the terminal width cannot be determined, a default width is
used.  This default width can be set with an option:

```
$ hr -d 78
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

If desired, the rule width can be specified:

```
$ hr -w 60
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

In cases where the terminal cannot display Unicode, ASCII may be used:

```
$ hr -a
------------------------------------------------------------------------------
```

The rule can include the current time:

```
$ hr -t
━━┫2021-05-27 19:26:09┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

The time format can be specified:

```
$ hr -t -f "%H:%M:%S.%q"
━━┫19:30:44.861779179000┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

The first line read from `STDIN` can be used as a note:

```
$ uname -m | hr -i
━━┫x86_64┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

When input is read, a timeout is used to ensure that `hr` does not "hang" when
there is no input.  The timeout (in milliseconds) can be specified:

```
$ uname -m | hr -i --timeout 100
━━┫x86_64┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

A note can be specified as one or more arguments:

```
$ hr unit tests
━━┫unit tests┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

Different types of notes can be combined:

```
$ uname -m | hr -it unit tests
━━┫2021-05-27 19:48:48┣━┫unit tests┣━┫x86_64┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

## Library

You can easily include horizontal rules in the output of your own Haskell
software by using the library.  The `hr` package is *not* in Hackage, however,
so you need to get it from the GitHub repository.

[Stack](https://www.haskellstack.org) users can add the dependency in
`extra-deps` of the `stack.yaml` file, specifying the commit hash for the
release.  Example:

```
resolver: lts-17.13

packages:
  - .

extra-deps:
  - github: ExtremaIS/hr-haskell
    commit: e48bb5047c53de29c45488a56ee35689fa59b0a1
```

[Cabal](https://www.haskell.org/cabal/) users can add the dependency as a
`source-repository-package` of the `cabal.project` file, specifying the commit
hash for the release as "`tag`".  (This feature is available from Cabal 2.4.)
Example:

```
packages: .

source-repository-package
    type: git
    location: https://github.com/ExtremaIS/hr-haskell.git
    tag: e48bb5047c53de29c45488a56ee35689fa59b0a1
```

A minimal example is provided in the [`example`](example) directory.

## Project

### Links

* GitHub: <https://github.com/ExtremaIS/hr-haskell>

### Releases

All releases are tagged in the `main` branch.  Release tags are signed using
the
[`security@extrema.is` GPG key](http://keys.gnupg.net/pks/lookup?op=vindex&fingerprint=on&search=0x1D484E4B4705FADF).

### Contribution

Issues and feature requests are tracked on GitHub:
<https://github.com/ExtremaIS/hr-haskell/issues>

Issues may also be submitted via email to <bugs@extrema.is>.

### License

This project is released under the
[MIT License](https://opensource.org/licenses/MIT) as specified in the
[`LICENSE`](LICENSE) file.
