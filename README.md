# hr

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![GitHub CI](https://github.com/ExtremaIS/hr-haskell/workflows/CI/badge.svg?branch=main)](https://github.com/ExtremaIS/hr-haskell/actions)
[![Hackage](https://img.shields.io/hackage/v/hr.svg)](https://hackage.haskell.org/package/hr)
[![Stackage LTS](https://stackage.org/package/hr/badge/lts)](https://stackage.org/package/hr)
[![Stackage Nightly](https://stackage.org/package/hr/badge/nightly)](https://stackage.org/nightly/package/hr)

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

`hr` can be built from source using [Stack][].  For example, you can install
the latest release (to `/usr/bin` on Linux) as follows:
as follows:

```
$ git clone https://github.com/ExtremaIS/hr-haskell.git
$ cd hr-haskell
$ make
$ sudo make install
```

[Stack]: <https://www.haskellstack.org>

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

The [hr Haskell library][] provides an API for easily including horizontal
rules in the output of your own software.

[hr Haskell library]: <https://hackage.haskell.org/package/hr>

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
