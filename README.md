# The standalone Ohua compiler: `ohuac`

[![.github/workflows/build.yml](https://github.com/ohua-lang/ohuac/actions/workflows/build.yml/badge.svg)](https://github.com/ohua-lang/ohuac/actions/workflows/build.yml)

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

## Developers

You may run individual test cases using stack like so:

    `stack test --ta '--match "/Rust Integration/TailRec/contexted function/"'`
