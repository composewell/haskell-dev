Getting Started with Haskell on Nix
===================================

See the `Haskell starting guide <getting-started.rst>`_ if
you are new to Haskell.  See the `Nix getting started guide
<getting-started-nix.rst>`_ first and then `Nix Package Management guide
<getting-started-nix-pkgs.rst>`_ if you are new to nix.

Query and Install Haskell Packages
----------------------------------

The default version of ``ghc`` is available directly under ``nixpkgs``
namespace::

  $ nix-env -iA nixpkgs.ghc

List all available Haskell compilers::

  $ nix-env -qaP -A nixpkgs.haskell.compiler

List all packages for a specific compiler version from the above list::

  $ nix-env -qaP -A nixpkgs.haskell.packages.ghc883

List all packages for the default compiler version::

  $ nix-env -qaP -A nixpkgs.haskellPackages

Note that ``haskellPackages`` is an alias to
``nixpkgs.haskell.packages.<default compiler version>``.

Installing Haskell packages::

  $ nix-env -iA nixpkgs.haskellPackages.streamly
  $ nix-env -iA nixpkgs.haskell.packages.ghc883.streamly

Nix Shell for Haskell Development
---------------------------------

We can use ``nix-shell`` to install ``ghc`` and required Haskell
packages.  In the shell we can use ``ghc`` and ``cabal`` as usual to
compile and build Haskell programs that depend on that ``ghc`` and the
packages we have installed with it.

Nix Shell with GHC
~~~~~~~~~~~~~~~~~~

Start a shell with ``ghc-8.6.5``::

  $ nix-shell --packages "haskell.compiler.ghc865"
  $ nix-shell --packages "[haskell.compiler.ghc865 haskell.packages.ghc865.mtl]"

Let's check what we have in the new shell::

  [nix-shell:~]$ ghc --version
  The Glorious Glasgow Haskell Compilation System, version 8.6.5

  [nix-shell:~]$ ghc-pkg list
  /nix/store/yh6ycb18aml15ybhsvnb4qxb6c7l28vp-ghc-8.6.5/lib/ghc-8.6.5/package.conf.d
      Cabal-2.4.0.1
      array-0.5.3.0
      base-4.12.0.0
      ...

haskellPackages.ghcWithPackages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can use the nix library function ``haskellPackages.ghcWithPackages``
to generate the list of packages along with the full set of their dependencies
from a given set of packages::

  $ nix-shell --packages "haskellPackages.ghcWithPackages (pkgs: with pkgs; [streamly mtl])"

It takes a function argument, the function is passed the set of all
Haskell packages (``pkgs``) and returns a list of packages (``[streamly
mtl]``) to be installed along with ``ghc``. ``ghcWithPackages`` would install
``ghc`` and register the packages passed along with all their dependencies with
ghc.

Nix Shell for a Hackage Package
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Each Haskell package (each nix derivation in fact) in the nix repository
has a corresponding prebuilt ``ghc`` shell environment available::

    $ nix-env -qaPA nixpkgs.haskellPackages.streamly.env
    nixpkgs.haskellPackages.streamly.env  ghc-shell-for-streamly-0.7.2

We can start a nix-shell by using the "<nixpkgs>" (which means find
``nixpkgs`` in ``NIX_PATH``) expression path and the attribute
identifying the shell environment under ``nixpkgs``::

  $ nix-shell -A haskellPackages.streamly.env "<nixpkgs>"

  [nix-shell:~]$

It creates a user environment with ``ghc`` and all the dependencies of the
package ``streamly`` installed but not the package itself. We can now use this
environment to develop the package::

  [nix-shell:~]$ which ghc
  /nix/store/y7zc21lk3l5v35w81qgz2mcrxwgf7601-ghc-8.8.3-with-packages/bin/ghc

See what you have got::

  [nix-shell:~]$ ghc-pkg list
  /nix/store/y7zc21lk3l5v35w81qgz2mcrxwgf7601-ghc-8.8.3-with-packages/lib/ghc-8.8.3/package.conf.d
      Cabal-3.0.1.0
      HUnit-1.6.0.0
      QuickCheck-2.13.2
      abstract-deque-0.3
      ...

shellFor
--------

shellFor starts a shell with ghc and Haskell or non-haskell dependencies
of given packages installed.

To add additional dependencies you can create a derivation with no source but
only dependencies and add it to the list of packages for shellFor. See
https://github.com/alpmestan/ghc.nix/blob/master/default.nix .

Haskell Packages
----------------

See this in nixpkgs/pkgs/top-level/all-packages.nix ::

  nixpkgs = {
    ...
    haskell = callPackage ./haskell-packages.nix { };
    haskellPackages = dontRecurseIntoAttrs haskell.packages.ghc883;
    ...
  }

See nixpkgs/pkgs/top-level/haskell-packages.nix::

  haskell = {
    ...
    lib
    compiler = {
      ...
      ghcjs
      ghcHead
      ghc883
      ...
  }

Haskell Library
---------------

Override the cabal/build config of a package.
``nixpkgs.haskell.lib.*`` see
nixpkgs/pkgs/development/haskell-modules/lib.nix::

  lib = {
    overrideCabal
    packageSourceOverrides

    do/dontCoverage
    do/dontHaddock
    doBenchmark/dontBenchmark
    ...

    add/append/removeConfigureFlag
    addBuildTool(s)
    addExtraLibrary
    addBuildDepend
    ...

    enableLibraryProfiling
    ...

    shellAware
  }

Haskell Derivations
-------------------

See nixpkgs/pkgs/top-level/haskell-packages.nix::

  haskell = {
    ...
    packageOverrides
    packages
    packages.ghc883.*
    ...
  }

``nixpkgs.haskell.packages.ghcxxx.*`` see 
nixpkgs/pkgs/development/haskell-modules/make_package_set.nix ::

  packages.ghcxxx = {
    override # Override the haskell package set
    extend # extend the haskell package set

    callHackage
    callHackageDirect
    callCabal2nixWithOptions
    callCabal2nix
    developPackage
    ghc
    ghcWithPackages
    ghcWithHoogle
    shellFor
  }

Custom Package Distribution
---------------------------

A custom package distribution is a bundle of packages for a specific task.
To make a custom package distribution, say ``nixpkgs.streamly-dev``, that can
be installed using ``nix-env`` like any other nix packages::

    $ cat ~/.config/nixpkgs/config.nix
    {
      packageOverrides =
          # the argument super would be "nixpkgs"
          super:
              let streamlyLibs = hpkgs:
                      with hpkgs;
                      [ # library dependencies
                        atomic-primops base containers deepseq directory
                        exceptions fusion-plugin-types ghc-prim heaps
                        lockfree-queue monad-control mtl network primitive
                        transformers transformers-base zlib
                        # test dependencies
                        ghc hspec QuickCheck random
                      ];
                  streamlyBenchmarks = hpkgs:
                      with hpkgs;
                      [ base deepseq exceptions gauge mtl random
                        transformers typed-process bench-show
                      ];
                  streamlyTools = hpkgs:
                      with hpkgs;
                      [ hlint hasktags ];
                  hDeps =
                      super.pkgs.haskell.packages.ghc883.ghcWithPackages
                          (hpkgs:
                              with hpkgs;
                              (  streamlyLibs hpkgs
                              ++ streamlyTools hpkgs
                              ++ streamlyBenchmarks hpkgs
                              )
                          );
              # return a set with new package definitions
              in { streamly-dev = hDeps; };
    }

Now we can search this package by the attribute ``nixpkgs.streamly-dev``::

    $ nix-env -qaPA nixpkgs.streamly-dev
    nixpkgs.streamly-dev  ghc-8.8.3-with-packages

We can also install this package using ``nix-env -iA nixpkgs.streamly-dev``.

Custom Nix Profile
------------------

We can use a dedicated nix profile for our development environment and
install our custom package distribution in this profile::

    $ nix-env -p ./streamly-dev -iA nixpkgs.nix
    $ nix-env -p ./streamly-dev -iA nixpkgs.streamly-dev

Now we can switch to our new profile to use the custom development
environment::

    $ nix-env -S ./streamly-dev
    $ ghc --version
    $ ghc-pkg list

Custom Nix Environment with Hoogle
----------------------------------

In our custom package distribution example, use ``ghcWithHoogle`` in
place of ``ghcWithPackages``.  When we install it, haddock documentation
and a hoogle database of all our Haskell packages in the distribution
is generated and installed at ``$HOME/.nix-profile/share/doc/hoogle/``.
Note that the ``hoogle`` binary in this profile is setup to pick the
database from this location instead of the standard ``~/.hoogle``.
The artifacts of interest in this directory are:

* The haddock docs: ``$HOME/.nix-profile/share/doc/hoogle/index.html``, use it by opening it in a browser
* The hoogle database:``default.hoo``, use it by running 
  ``hoogle server --local -p 8080``

Nix Build for a local Package
-----------------------------

`cabal2nix`` converts ``<package>.cabal`` to ``<package>.nix`` nix
expression file. Install ``cabal2nix`` ::

  $ nix-env -i cabal2nix
  $ cabal2nix --version
  cabal2nix 2.15.3
  $ cabal2nix --help

Convert the ``.cabal`` file of your package to ``.nix`` file::

  $ cabal2nix . > streamly.nix

Note that we used ``.`` in the argument above. If you specify the
``streamly.cabal`` file instead of ``.`` then it generates the nix file
from Hackage.

Create a ``default.nix`` to run ``nix-build`` conveniently using the nix
file generated above::

  $ cat > default.nix
  { nixpkgs  ? import <nixpkgs> {}
  , compiler ? "ghc865"
  }:
  nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./<package>.nix { }

  $ nix-build

Nix Shell for a local Package
-----------------------------

Create a custom shell environment for a local package::

  $ cabal2nix --shell . > shell.nix

  $ nix-shell

  [nix-shell]$ ghc-pkg list
  [nix-shell]$ cabal build

Since nix shell already installed all the dependencies and registers them
with ``ghc``, ``cabal build`` does not build any dependencies, it just
builds the current package using the pre-installed dependencies.

If you want to add additional packages, you need to exit the shell, add the new
package to ``shell.nix``, and restart the shell.

Environment variables inherited from the current shell can still influence the
build in the nix shell. To make sure that the environment is cleared in the nix
shell::

  $ nix-shell --pure

Note that with this option ``.bashrc`` (or the rc file of your shell) is
still run.

To use a different compiler than the one specified in ``shell.nix``::

  $ nix-shell --argstr compiler ghc865

Nix shell for a multi-package project
-------------------------------------

* https://gist.github.com/codebje/000df013a2a4b7c10d6014d8bf7bccf3
* https://input-output-hk.github.io/haskell.nix/reference/library/#callcabalprojecttonix

Overriding a Nix installed package
----------------------------------

If you want to use a custom version of a package instead of the one
available from the nix channel.  Generate a nix expression that will be
used to override the package. Use `--no-check` flag if you want to avoid
running tests for the package::

  $ cabal2nix --no-check cabal://streamly-0.6.1 > ~/.nixpkgs/streamly-0.6.1.nix

Then add an override in `default.nix` for your package as follows::

  {
    packageOverrides = super:
      let self = super.pkgs;
      in {
        haskell = super.haskell // {
          packages = super.haskell.packages // {
            ghc865 = super.haskell.packages.ghc865.override {
              overrides = self: super: {
                streamly = self.callPackage ./streamly-0.6.1.nix {};
              };
            };
          };
        };
      };
  }

``.`` refers to ``~/.nixpkgs``?

Building haskell packages without doCheck::

  nixpkgs = import (builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/dcb64ea42e6.tar.gz)
      { overlays =
          [(self: super: {
                haskell = super.haskell // {
                  packageOverrides = hself: hsuper: {
                    mkDerivation = args: hsuper.mkDerivation (args // {
                      doCheck = false;
                    });
                  };
                };
              }
           )
          ];
      };

Global Override
---------------

Add the above expression in `~/.config/nixpkgs/config.nix` to override
package versions used in a package set.

Using a source repository package
---------------------------------

To use the git source of the streamly package and an external
library dependency on `zlib`, in your `default.nix` file use
`compiler.developPackage`::

  { compilerVersion ? "ghc865" }:
  let
    pkgs = import (fetchGit (import ./version.nix)) { };
    compiler = pkgs.haskell.packages."${compilerVersion}";
    pkg = compiler.developPackage {
      root = ./.;
      source-overrides = {
        streamly = "0.6.1";
      };
    };
    buildInputs = [ zlib ];
  in pkg.overrideAttrs
      (attrs: { buildInputs = attrs.buildInputs ++ buildInputs;})


Cabal
-----

Run cabal commands using the nix shell environment defined in your
``shell.nix`` file ::

  cabal --enable-nix ...

Stack
-----

Build with stack using your nix environment ::

  stack --nix build

Reference Material
------------------

Haskell Attributes
~~~~~~~~~~~~~~~~~~

::

  nixpkgs.haskell
  nixpkgs.haskell.compiler
  nixpkgs.haskell.packages
  nixpkgs.haskell.packages.ghc865
  nixpkgs.haskell.packages.ghc883
  ...
  nixpkgs.haskellPackages

Haskell mkDerivation
--------------------

To dig into Haskell ``mkDerivation`` attributes, see::

    ~/.nix-defexpr/channels/nixpkgs/pkgs/development/haskell-modules/generic-builder.nix

Haskell build functions
-----------------------

::

    haskellPackages.ghcWithPackages
    haskellPackages.ghcWithHoogle

Diagnostics
-----------

Important: Multiple packages/libraries with the same name may be
available in different namespaces and under the nix expression
(repository), make sure that you are linking with the correct
library. For example, there is a ``nixpkgs.pkgs.zlib`` and a
``nixpkgs.haskellPackages.zlib``, both are different things and
sometimes using one for the other works but may produce strange results
or errors.

Q: "Missing dependency on a foreign library" when using the nix installed GHC
outside nix.  To resolve this:

A: Do any of the following:
* Use `cabal --enable-nix`, assuming ``shell.nix`` provides the library
* Use a nix shell environment with the given library installed
* Provide the lib/include dir options as shown below

Find the nix-path for the library (e.g. zlib)::

  $ nix-build --no-out-link "<nixpkgs>" -A zlib

Then use this path in `--extra-lib-dirs=` and `--extra-include-dirs=` options
of cabal.

You can also install the library in the nix user's profile using `nix-env` and
use `LIBRARY_PATH` environment variable to tell gcc/clang about it::

  $ export LIBRARY_PATH=$HOME/.nix-profile/lib

Other environment variables that can be used to affect gcc/clang::

  $ export C_INCLUDE_PATH=$HOME/.nix-profile/include
  $ export CPLUS_INCLUDE_PATH=$HOME/.nix-profile/include

Q: When compiling with ghc/gcc/clang I see an error like this::

    Linking .../streamly-benchmarks-0.0.0/x/chart/build/chart/chart ...
    ld: library not found for -lz
    clang-7: error: linker command failed with exit code 1 (use -v to see invocation)
    `cc' failed in phase `Linker'. (Exit code: 1)
    Error: build failed

A: ``libz`` (``-lz`` in the error message) is provided by ``nixpkgs.pkgs.zlib``.
   Add ``nixpkgs.pkgs.zlib`` to ``executableSystemDepends`` in ``mkDerivation``.

Mac OS Specific
~~~~~~~~~~~~~~~

Q: Got a "framework not found" error when linking an executable::

  Linking .../streamly-benchmarks-0.0.0/x/chart/build/chart/chart ...
  ld: framework not found Cocoa
  clang-7: error: linker command failed with exit code 1 (use -v to see invocation)
  `cc' failed in phase `Linker'. (Exit code: 1)
  Error: build failed

A: Add ``nixpkgs.pkgs.darwin.apple_sdk.frameworks.Cocoa`` to
  ``executableSystemDepends`` in mkDerivation.

Unresolved Issues
-----------------

Q: When building streamly local package with ghc883, it fails with::

  compileBuildDriverPhase
  setupCompileFlags: -package-db=/private/var/folders/p4/fdt36vy95f52t_3dnpcx8_340000gn/T/nix-build-streamly-0.7.2.drv-0/setup-package.conf.d -j8 -threaded -rtsopts
  Loaded package environment from /private/var/folders/p4/fdt36vy95f52t_3dnpcx8_340000gn/T/nix-build-streamly-0.7.2.drv-0/streamly/.ghc.environment.x86_64-darwin-8.8.3
  [1 of 1] Compiling Main             ( Setup.hs, /private/var/folders/p4/fdt36vy95f52t_3dnpcx8_340000gn/T/nix-build-streamly-0.7.2.drv-0/Main.o )

  Setup.hs:3:1: error:
      Could not load module ‘Distribution.Simple’
      It is a member of the hidden package ‘Cabal-3.0.1.0’.
      You can run ‘:set -package Cabal’ to expose it.
      (Note: this unloads all the modules in the current scope.)
      Use -v (or `:set -v` in ghci) to see a list of the files searched for.
    |
  3 | import Distribution.Simple
    | ^^^^^^^^^^^^^^^^^^^^^^^^^^

  builder for '/nix/store/n615rlmr0lkkdjh84ymgh3hcrcibyp5j-streamly-0.7.2.drv' failed with exit code 1

A: Edited
``nixpkgs/pkgs/development/haskell-modules/generic-builder.nix`` to add
``-package Cabal`` flag when compiling but then it starts compiling the whole
world including ghc.

Compiling GHC
-------------

override gmp to install header file.

::

  export C_INCLUDE_PATH=~/.nix-profile/include

Quick References
----------------

* `Nix getting started guide <getting-started-nix.rst>`_
* `Nix manual Haskell section <https://nixos.org/nixpkgs/manual/#haskell>`_
* `cabal2nix: convert cabal file to nix expression <http://hackage.haskell.org/package/cabal2nix>`_
* `hackage2nix: update Haskell packages in nixpkgs <https://github.com/NixOS/cabal2nix/tree/master/hackage2nix>`_

Resources
---------

* https://github.com/input-output-hk/haskell.nix
* https://github.com/cachix/cachix-action
* https://stackoverflow.com/questions/57725045/disable-building-and-running-tests-for-all-haskell-dependencies-in-a-nix-build
* https://github.com/direnv/direnv/wiki/Nix direnv
* https://github.com/target/lorri/ lorri
