# `hr-haskell` `0.3.0.0` Release Notes

Date
: 2021-05-28

## Overview

This is a major release of `hr`, adding a library and tests, as well as adding
new functionality to the CLI.

The library allows you to easily include horizontal rules in out output of
your Haskell software.

The CLI now includes an option to read a note from `STDIN`.  When input it
read, a timeout is used to ensure that `hr` does not "hang" when there is no
input.  There are also new options for specifying a fixed rule width as well
as the default rule width to use when the width of the terminal cannot be
determined.
