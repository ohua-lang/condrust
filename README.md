# The standalone Ohua compiler: `ohuac`

[![.github/workflows/build.yml](https://github.com/ohua-lang/condrust/actions/workflows/build.yml/badge.svg)](https://github.com/ohua-lang/condrust/actions/workflows/build.yml)

## Usage

For a complete list of the options available use the `--help` flag.

## Building from source

Required tools:

- `stack` https://haskellstack.org

### Instructions

1. Clone the repository
```
git clone https://github.com/ohua-dev/condrust
```

2. Build the program

```
stack install
```

   This downloads and builds all dependencies, as well as the Haskell compiler
   `ghc`, should it not be present already. It should not interfere with any
   system-level libraries, Haskell ecosystems etc.

   It builds the executable `condrust` and copies it to `~/.local/bin`. If you do
   not wish this use `stack build` instead and find the path of the binary using
   `stack exec -- which condrust` afterwards


## Examples

Check out the test cases in the `test` folder for our existing integrations.

## Developers

You may run individual test cases using stack like so:

```
stack test --ta '--match "/Rust Integration/TailRec/contexted function/"'
```

Also, we do run our CI based on Nix. For development, you may want to build and run tests without Nix.
You can do so via
```
stack build --no-nix
stack test --no-nix
```
You can read up on it in the (section on Nix integration of the stack documentation)[https://docs.haskellstack.org/en/stable/nix_integration/].

# The core of ohua -- basic types and functionalitites

## Information for Developers

### Documentation

The official, verbose documentation is on
[readthedocs](https://ohua.readthedocs.org).

In addition many of the parts of the compiler library are documented with the
documentation tool [haddock](https://haskell.org/haddock).

To get a browsable documentation locally use `stack hoogle -- serve --local`.
This will build the documentation for both the core, as well as its
dependencies and start a server locally on port `8080`. There you can search for
functions, types, modules and libraries and browse their documentation.

### Code formatting

The default formatting for code is done using the `hindent` library, the
configuration file `.hindent.yaml` can be found at the project root.

### Notes on Universum

I use a `Prelude` replacement called `Universum` in this project. This has a few
minor consequences for how code is written. Most importantly I activate the
`NoImplicitPrelude` extension by default. This means you have to import a
prelude explicitly.

1. You can use `Ohua.Commons.Prelude` which pulls in `Universum` as well as basic ohua
   core modules (`Ohua.Types`, `Ohua.Commons.Util`) and code traversal helpers such as
   `transform` and `rewrite`.
2. Alternatively you can import `Universum`
3. or `Prelude ` if you *really* like `Prelude`).

The following are some notes on using `Universum`, assembled for your convenience:

- Be very careful when using `[]` in your code. 

  If your module also includes `NonEmpty` then this will always result in a runtime 
  error! The reason for that is because we have the `OverloadedLists` extension enabled
  and `NonEmpty` implements an instance of `IsList`. So `[]` desugars into `NE.fromList []`
  which will always fail!

- `error` from `Universum` uses `Text` rather than `String`

  This should not be an issue, most value we manipulate in this library are
  `Text` anyways.

- Trace functions (`trace`, `traceShowId` etc) are in scope by default, but
  raise a warning.

  Aka you can use them, but you have to remove them if you want the code to pass
  the CI.

- Universum uses `Text` rather than `String` by default.

  Aka use `<>` (also in scope by default) rather than `++` to concatenate
  strings.

- some IO is polymorphic

  `putStrLn "some string"` will raise an ambiguity error, you'll have to
  annotate the string like so `putStrLn ("some string" :: Text)`

- `mtl` stuff is in scope by default.

  The `Reader`, `ExceptT` and `State` monad (+ transformers, + classes) are in
  scope by default. No need to import `Control.Monad.Reader` etc


For more information on universum check out [the GitHUb
repository](https://github.com/serokell/universum).


## Acknowledgements

`dfg-mlir` has been supported throughout its history by the following projects.

European Union projects:

- Grant agreement ID 957269 **EVEREST** – dEsign enVironmEnt foR Extreme-Scale big data analytics on heterogeneous platforms

