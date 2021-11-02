# The standalone Ohua compiler: `ohuac`

<!-- [![Build Status](https://travis-ci.org/ohua-dev/ohuac.svg?branch=master)](https://travis-ci.org/ohua-dev/ohuac) -->

For the complete documentation of the `ohuac` compiler see the [standalone
compiler section of the ohua
documentation](https://ohua.readthedocs.org/en/latest/ohuac.html)

## Usage

For a complete list of the options available use the `--help` flag.

## Building from source

Required tools:

- `stack` https://haskellstack.org

### Instructions

1. Clone the repository

    `git clone https://github.com/ohua-dev/ohuac`

2. Build the program

   `stack install`

   This downloads and builds all dependencies, as well as the Haskell compiler
   `ghc`, should it not be present already. It should not interfere with any
   system-level libraries, Haskell ecosystems etc.

   It builds the executable `ohuac` and copies it to `~/.local/bin`. If you do
   not wish this use `stack build` instead and find the path of the binary using
   `stack exec -- which ohuac` afterwards


## Examples

Check out the test cases in the `test` folder for our existing integrations.
