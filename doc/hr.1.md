---
title: HR
section: 1
hyphenate: false
...

# NAME

`hr` - horizontal rule for the terminal

# SYNOPSIS

`hr` [*OPTIONS*] [NOTE ...]

# DESCRIPTION

`hr` is a utility for displaying a horizontal rule in a terminal.

# OPTIONS

-h, \--help
:   show help and exit

\--version
:   show version and exit

-w, \--width *CHARS*
:   target rule width (default: terminal width)

-d, \--default *CHARS*
:   default target rule width (default: 80)

-a, \--ascii
:   use ASCII lines (default: use Unicode lines)

-t, \--time
:   show time

-f, \--format *FORMAT*
:   time format (default: `%Y-%m-%d %H:%M:%S`)

    The following format codes are supported:

    * `%Y` - four-digit year
    * `%y` - two-digit year
    * `%m` - two-digit month
    * `%d` - two-digit day
    * `%H` - two-digit hour using 24-hour clock
    * `%I` - two-digit hour using 12-hour clock
    * `%p` - locale equivalent of AM or PM
    * `%M` - two-digit minute
    * `%S` - two-digit second
    * `%f` - six-digit microsecond
    * `%z` - UTC offset

-i, \--input
:   read note from STDIN within MS milliseconds

\--timeout *MS*
:   timeout in milliseconds (default: 500)

# ARGUMENTS

*NOTE*
:   show a note

# EXIT CODES

0
:   no error

2
:   command-line error

# PROJECT

GitHub:
:   <https://github.com/ExtremaIS/hr-haskell>

Reporting issues:
:   GitHub: <https://github.com/ExtremaIS/hr-haskell/issues>

    Email: <bugs@extrema.is>

Copyright
:   Copyright (c) 2019-2021 Travis Cardwell

License
:   The MIT License <https://opensource.org/licenses/MIT>
