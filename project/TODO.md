# `hr-haskell` TODO

## Functionality

## Tests

## Compatibility

* GHC 9.8.3 is not distributed by GHCup, which is used by `haskell-actions`.
  I have not had time to update the CI configuration to test it specially, but
  GHC 9.8.4 will thankfully soon be released.
* GHC 9.10.1 is now supported, by vendoring problematic dependencies.  This
  requires releasing a new version of the project.  I plan on doing this after
  support for GHC 9.8.4 is complete.

## Documentation

## Examples

## Project

* Decide what to do about `HMock`
    * Remove tests?
    * Switch to a different mock library?
    * Rewrite using an effects library?
