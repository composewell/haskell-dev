# GHC Cabal Interface

In [](ghc-packages.md) we explained how GHC manages packages and package
databases. The GHC installation provides the essential `ghc-pkg` tool to work
with the package databases.

In this document we will explain the
[Cabal](https://hackage.haskell.org/package/Cabal) library that provides a low
level interface for building packages and registering, unregistering
packages with GHC so that it can use those. Higher level build systems
like `cabal` and `stack` depend on this library to actually build and
install the packages.

## A simple build program

Using the [Cabal](https://hackage.haskell.org/package/Cabal) package we
can write the following build program.

```
$ cat > Setup.hs
module Main (main) where

import Distribution.Simple

main :: IO ()
main = defaultMain
```

Usually we have a `Setup.hs` file in each package with something similar
to this. It provides a low level way to build the package. Higher level
build packages use this to build the package if it is present. We can
build the package using this without having to use a higher level build
system like `cabal` or `stack`. For example, if you are using Nix you can just
work with this simple build tool.

If we compile and run it:

```
$ ./Setup --help

This Setup program uses the Haskell Cabal Infrastructure.
See http://www.haskell.org/cabal/ for more information.

Usage: Setup [GLOBAL FLAGS] [COMMAND [FLAGS]]

Commands:
  configure          Prepare to build the package.
  build              Compile all/specific components.
  show-build-info    Emit details about how a package would be built.
  repl               Open an interpreter session for the given component.
  install            Copy the files into the install locations. Run register.
  copy               Copy the files of all/specific components to install locations.
  doctest            Run doctest tests.
  haddock            Generate Haddock HTML documentation.
  clean              Clean up after a build.
  sdist              Generate a source distribution file (.tar.gz).
  hscolour           Generate HsColour colourised code, in HTML format.
  register           Register this package with the compiler.
  unregister         Unregister this package with the compiler.
  test               Run all/specific tests in the test suite.
  bench              Run all/specific benchmarks.
  help               Help about commands.

For more information about a command use
  Setup COMMAND --help

Typical steps for installing Cabal packages:
  Setup configure
  Setup build
  Setup install
```

We have got a minimal build system to build our package. This build system
assumes that you have already installed the dependencies of the package. It
would just use those as per the specs in the cabal file or fail.

### Configure

The first step to build is invoking `Setup configure`. It collects build
information and creates a buildinfo file to be used by subsequent build phases.

```
$ ./Setup configure --help
$ ./Setup configure
```

By default it creates a `setup-config` file in the `dist` directory (can be
overridden by --builddir option). This file basically contains
[LocalBuildInfo](https://hackage.haskell.org/package/Cabal/docs/Distribution-Types-LocalBuildInfo.html#t:LocalBuildInfo).
If you want to see the contents of this file:

```
$ ./Setup show-build-info
```

