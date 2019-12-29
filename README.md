# hr

[![Build Status](https://travis-ci.com/ExtremaIS/hr-haskell.svg?branch=master)](https://travis-ci.com/ExtremaIS/hr-haskell)

* [Overview](#overview)
* [Requirements](#requirements)
* [Installation](#installation)
    * [Installation From Source](#installation-from-source)
* [Usage](#usage)
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

## Requirements

`hr` has only been tested on Linux.  It *might* work on Windows and OS X.

## Installation

### Installation From Source

`hr` can be built from source using [Stack](https://www.haskellstack.org).
For example, you can install the latest release (to `~/.local/bin` on Linux)
as follows:

```
$ git clone https://github.com/ExtremaIS/hr-haskell.git
$ cd hr-haskell
$ stack install
```

## Usage

See the [`hr` man page](doc/hr.1.md) for usage information.

## Project

There are no plans to put this package on Hackage.

### Links

* GitHub: <https://github.com/ExtremaIS/hr-haskell>

### Releases

All releases are tagged in the `master` branch.  Release tags are signed using
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
