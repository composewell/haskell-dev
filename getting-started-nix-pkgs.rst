Nix Package Management
======================

If you are new to nix read the `Nix getting started guide
<getting-started-nix.rst>`_ first.

Nix is a package manager, you can build, install, use and remove
packages from different sources into the system hosting nix. Before
building a package, it builds and installs all the necessary
dependencies of the package, if necessary. Moreover, the dependencies of
one package cannot affect other packages, each package can have its own
version of the same dependency.

Package Management Process
--------------------------

Describing the Package
~~~~~~~~~~~~~~~~~~~~~~

You can easily build and optionally install your own package with
nix. To describe a package to nix you need to create a directory and add
a ``default.nix`` file in it. The key information that ``default.nix``
contains is how to fetch the sources of the package and how to build
it. We may optionally need a build script to help build the package.

Building the Package
~~~~~~~~~~~~~~~~~~~~

``nix-build`` takes ``default.nix`` and executes the recipe defined in
it. It can (optionally) fetch the sources in a temporary directory,
run the build script, create one or more output directories, and pass the
paths of those to the build script. The build script would then build
the package and place the outputs in those directories. The output directory is
symlinked as ``result`` in the current directory.

Installing the Package
~~~~~~~~~~~~~~~~~~~~~~

If we hook our package into the nix package hierarchy, we can use
``nix-env`` to install the package. It would do what ``nix-build`` does
but instead of creating a ``result`` symlink it makes the executables or
libraries available in the current nix profile via ``~/.nix-profile``.

A Trivial Package
-----------------

``default.nix`` defines a nix expression to describe the package and how
to build it.  The nix expression defines a build function that may take
some inputs and produce some outputs. The build function is also called
a derivation, as it derives output artifacts using some input artifacts.

Let's create a trivial package first::

  $ mkdir nix-trivial
  $ cd nix-trivial
  $ cat > default.nix
  {}:
    derivation {
        name = "dummy";
        system = "x86_64-darwin";
        builder = "/usr/bin/env";
        src = ./.;
    }

This file defines a function with no arguments. A function in nix is
defined as ``{ arg1, arg2, ..., argn }: expr`` where ``arg1``, ``arg2``,
and ``argn`` are arguments to the function and ``expr`` is the body of
the function.

Our function body calls another function ``derivation`` with a
``set`` type argument (a set of key value pairs) describing the
package. Our ``dummy`` package is built using the ``/usr/bin/env``
program. ``/usr/bin/env`` does nothing, it just prints its environment
variables. We have chosen this just to illustrate the environment that
nix uses to call the builder, which is important to write a real builder.

Environment of the builder
~~~~~~~~~~~~~~~~~~~~~~~~~~

Let's build this package using ``nix-build``::

  $ nix-build
  ...
  NIX_BUILD_CORES=8
  NIX_LOG_FD=2
  NIX_STORE=/nix/store
  TERM=xterm-256color

  HOME=/homeless-shelter
  PATH=/path-not-set

  NIX_BUILD_TOP=/private/var/folders/p4/fdt36vy95f52t_3dnpcx8_340000gn/T/nix-build-dummy.drv-0
  PWD=/private/var/folders/p4/fdt36vy95f52t_3dnpcx8_340000gn/T/nix-build-dummy.drv-0
  TEMP=/private/var/folders/p4/fdt36vy95f52t_3dnpcx8_340000gn/T/nix-build-dummy.drv-0
  TEMPDIR=/private/var/folders/p4/fdt36vy95f52t_3dnpcx8_340000gn/T/nix-build-dummy.drv-0
  TMP=/private/var/folders/p4/fdt36vy95f52t_3dnpcx8_340000gn/T/nix-build-dummy.drv-0
  TMPDIR=/private/var/folders/p4/fdt36vy95f52t_3dnpcx8_340000gn/T/nix-build-dummy.drv-0
  ...

In addition, nix also passes the attributes in the argument set of
``derivation`` as environment variables with the same names::

  ...
  name=dummy
  system=x86_64-darwin
  builder=/usr/bin/env
  ...

Lastly, it passes a default ``out`` environment variable pointing to a
directory where the builder is supposed to store its output artificats::

  ...
  out=/nix/store/2869jzplqdaipayhij966s3c5lxv83l3-dummy
  ...

Notice that nix cleans the environment before invoking the builder
process and sets only those variables that are strictly required and
even sets some of the variables (``HOME`` and ``PATH``) to "junk" values
so that defaults are not filled by the shell. This is to ensure an
isolated build environment. We used ``/usr/bin/env`` in this example for
illustration, but we are not supposed to use any path outside the nix
sandbox for building, we must have explicit dependencies on other nix
packages and use the paths of those.

Local Path Translation
~~~~~~~~~~~~~~~~~~~~~~

Another important thing to note is that we have an attribute ``src =
./.`` referring to the current directory path. Any path type attribute
referring to a local path causes the file or the directory tree to
be copied to the store and its location in the store is put in the
environment variable::

  src=/nix/store/cxs2idim7nrlxn100c7nk0s91pr17csm-nix-trivial

Also, note that the permissions of the tree are made read-only and the
timestamps are set to 01-Jan-1970.

We can access any artifacts in our current directory by using the above
translated path.

Derivation Spec
~~~~~~~~~~~~~~~

``nix-build`` first creates a spec for the derivation which is stored in the
nix store::

  these derivations will be built:
    /nix/store/fyfjlgp5xzkdhkzp32hvcbxdijaimkxq-packcheck-0.5.1.drv
  building '/nix/store/fyfjlgp5xzkdhkzp32hvcbxdijaimkxq-packcheck-0.5.1.drv'...

We can open this file and see all the details of this derivation.

An Example Package
------------------

Let's now try to build a small real Haskell source package. `packcheck
<http://hackage.haskell.org/package/packcheck>`_ is a minimal Haskell
package that contains a shell script ``packcheck.sh`` which can build
any Haskell package. We will use that script to build ``packcheck`` itself::

  $ mkdir nix-play
  $ cd nix-play
  $ cat > default.nix
  {}:
      with import <nixpkgs> {};
      let src = fetchurl {
            url = http://hackage.haskell.org/package/packcheck-0.5.1/packcheck-0.5.1.tar.gz;
            sha256 = "79e7cfc63e70b627be8c084b3223fdd261a5d79ddd797d5ecc2cee635e651c16";
          };

          path =
                "${bash}/bin"
              + ":${which}/bin"
              + ":${coreutils}/bin"
              + ":${gnused}/bin"
              + ":${gawk}/bin"
              + ":${gnutar}/bin"
              + ":${gzip}/bin"
              + ":${curl}/bin"
              + ":${llvmPackages.bintools}/bin"
              + ":${ghc}/bin"
              + ":${cabal-install}/bin";

      in derivation {
          name = "packcheck-0.5.1";
          system = "x86_64-darwin";
          builder = "${bash}/bin/bash";
          args =
              [ "-c"
                ''set -e
                  export HOME=$TMP
                  export PATH=${path}
                  tar -zxvf ${src}
                  cd packcheck-0.5.1
                  bash packcheck.sh cabal-v2
                  mkdir -p $out/bin
                  touch $out/bin/hello
                ''
              ];
      }

``with`` is a nix language keyword. ``import``, ``fetchurl`` and
``derivation`` are nix builtin functions. We can use them with or without
``builtins.`` prefix e.g. ``builtins.import`` or just ``import``.

``<nixpkgs>`` is a syntax to refer to the first nix module (better known
as nix expression) by the name ``nixpkgs`` found in ``NIX_PATH``. If you
are familiar with the ``C`` language then this is similar to the ``include
<stdio.h>`` syntax. 

``with import <nixpkgs> {};`` brings all the child modules of
``nixpkgs`` into the current scope. For example, the package
``nixpkgs.ghc`` comes into the current scope as the name ``ghc`` and we
can refer to it using ``${ghc}``.

``builtins.fetchurl`` downloads the file referred to by the URL and assigns
the path location of the downloaded file to the ``src`` variable.

We setup the ``path`` variable to a ``PATH`` string containing the paths of all
the required utilities needed by the build script.

``derivation`` uses ``bash`` as the builder which is invoked with the
``-c`` option passing an inline bash script as argument. The script
untars the source tarball, changes directory to the source and then
invokes its build script ``packcheck.sh`` to build the package. Finally,
it creates a dummy ``hello`` artifact inside the output directory passed
by nix.

Build output
~~~~~~~~~~~~

We can run ``nix-build`` to build our package::

    $ nix-build

It creates a ``result`` symlink which points to the output directory that was
passed to the build script as the ``out`` environment variable::

    $ ls -al result
    lrwxr-xr-x  1 harendra  wheel  59 Jun 15 20:23 result -> /nix/store/i1ki0dh1fgd1rcs0ljbzak6ilmymzldp-packcheck-0.5.1

callPackage
~~~~~~~~~~~

In the above example, for simplicity we used ``with import <nixpkgs> {}``
which brought all the package names under ``nixpkgs`` as variables
in our scope.  Instead of clobbering the namespace with all those
variables we should pass them as arguments, as follows::

  $ cat packcheck.nix
  { fetchurl, bash, which, coreutils, gnused, gawk, gnutar, gzip, curl
  , llvmPackages, ghc, cabal-install }:
  ...

Then we can call the function in ``packcheck.nix`` supplying the
arguments using ``nix-build`` as follows::

    $ nix-build -E 'with import <nixpkgs> {}; nixpkgs.pkgs.callPackage ./packcheck.nix {}'

``callPackage`` calls ``./packcheck.nix``, automatically filling the
arguments that are not explicitly supplied in the arguments to
``callPackage`` (i.e. ``{}`` in the above example). The argument
variables are filled from the variables of the same names available in
the current scope i.e. the ones brought in scope by the ``with`` clause
in the command above.

We can write this expression in ``default.nix`` so that we can use
``nix-build`` without any arguments::

  $ cat default.nix
  { nixpkgs ? import <nixpkgs> {} }:
      nixpkgs.pkgs.callPackage ./packcheck.nix {}
  $ nix-build

Adding to Nix Packages
~~~~~~~~~~~~~~~~~~~~~~

TBD

That's it. The rest of this guide contains reference material to
understand the Nix expression language and library functions. You
are now equipped with all the basic knowledge of Nix and Nix
packaging, you can now move on to the `Nix Haskell Guide
<getting-started-nix-haskell.rst>`_.

Nix Expression Language
-----------------------

You can use ``nix repl`` to try out the language interactively.

Comments::

    # this is a comment
    /* this is a comment */

Strings::

    "hello world"
    'hello world'

Indented Strings::

  ''
    This is the first line.
    This is the second line.
      This is the third line.
  ''

URI Strings can be written without quotes::

    http://hello.world

Expand an expression in a string::

    let expr = "world" in "hello ${expr}"

Paths (at least one "/" is needed)::

    ./.       # current directory
    /.        # root directory
    a/b
    ~/a       # expand ~ to home directory
    <nixpkgs> # search nixpkgs in NIX_PATH

Booleans: ``true``, ``false``
Null : ``null``

Define local variables::

  let
    x = "foo";
    y = "bar";
  in x + y

Lists: whitespace separated items in square brackets::

    [ 123 ./foo.nix "abc" (f { x = y; }) ]

Adding lists::

    [ 1 2 ] ++ [ 3 4 ]

Sets: name value pairs (attributes), terminated by ``;`` and enclosed in
curly brackets::

  { x = 123;
    text = "Hello";
    y = f { bla = 456; };
  }

Attributes can be selected from a set using the ``.`` operator.  Default
value in an attribute selection can be provided using the ``or``
keyword. For example::

  { a = "Foo"; b = "Bar"; }.c or "Xyzzy"

A set that has a ``__functor`` attribute whose value is callable (i.e. is
itself a function or a set with a __functor attribute whose value is
callable) can be applied as if it were a function, with the set itself
passed in first::

  let add = { __functor = self: x: x + self.x; };
      inc = add // { x = 1; };
  in inc 1

Inherit: In a set or in a let-expression definitions can be inherited::

  let x = 123; in
  { x = x;
    y = 456;
  }

  is equivalent to

  let x = 123; in
  { inherit x;
    y = 456;
  }

``inherit x`` implies ``x = x``
``inherit (pkgs) zlib`` implies ``zlib = pkgs.zlib``

Aninymous functions are defined as ``pattern: body``::

    # single argument function
    x: !x # negation function

    # set argument
    { x, y, z }: x + y + z

    # optional arguments with default values
    { x, y ? "foo", z ? "bar" }: x + y + z

    # @pattern, in the following examples "args" variable holds the
    # whole argument set
    args@{ x, y, z, ... }: x + y + z + args.a
    { x, y, z, ... } @ args: x + y + z + args.a

Named functions are just let bindings for anonymous functions::

    let f = x: !x
        g = {x , y, z}: x + y + z

Calling a function::

    # single argument
    f "foo"

    # set argument
    f {x = "foo"; y = "bar"; z = "baz";}

First class functions (functions returning functions)::

    let concat = x: y: x + y; # function returning a function
    in builtins.map (concat "foo") [ "bar" "bla" "abc" ] # Currying

Conditionals::

    if e1 then e2 else e3

Assertions::

    assert e1; e2

with::

``with set; expr``: introduces the set ``set`` into the lexical scope of
``the expression expr``::

  let as = { x = "foo"; y = "bar"; };
  in with as; x + y

  with (import ./definitions.nix); ...

It can also be written as::

  with import ./definitions.nix; ...

Function application: In the expression::

  with import <nixpkgs> {};

``with import`` returns a function which consumes the next argument, which
returns a function which consumes the next argument.

Built-in functions
------------------

Nix provides `a library of built-in functions
<https://nixos.org/nix/manual/#ssec-builtins>`_. All built-in functions are
available through the ``builtins.`` namespace prefix. Some common functions
are available even without that prefix.

``builtins.import path`` Load, parse and return the Nix expression in
the file ``path``. If ``path`` is a directory, the file ``default.nix``
in that directory is loaded.

Derivation
----------

`builtins.derivation <https://nixos.org/nix/manual/#ssec-derivation>`_ is a
function to build a package::

    derivation {
        name    # package name
        system  # e.g. "i686-linux" or "x86_64-darwin"
        builder # build script, a derivation or a path e.g. ./builder.sh
        args ? []    # command line args to be passed to the builder
        outputs ? [] # a list of symbolic outputs of the derivation
                     # e.g.  [ "lib" "headers" "doc" ]
    }

Builder Environment and Execution
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Debugging Note: We can use ``/usr/bin/env`` as the builder script to print the
environment that is being passed to the builder.

Every attribute of ``derivation`` is passed as an environment variable
to the builder process with the following translations:

* A path (e.g., ../foo/sources.tar) type attribute causes the referenced
  file to be copied to the store; its location in the store is put in the
  environment variable.

  The tree copied in the nix store is made read-only. If the builder depends on
  the ability to write to this tree in-place then it has to make it writable
  explicitly. Or it has to copy the tree to the temporary directory.

  The copied tree in the nix store has timestamps as 01-Jan-1970, the
  beginning of the Unix epoch. So you cannot depend on the timestamps.
* A derivation type attribute causes that derivation to be built prior
  to the present derivation; its default output path is put in the
  environment variable.
* ``true`` is passed as the string ``1``, ``false`` and ``null`` are
  passed as an empty string.
* By default, a derivation produces a single output path, denoted
  as ``out``. ``outputs = [ "lib" "headers" "doc" ]`` causes ``lib``,
  ``headers`` and ``doc`` to be passed to the builder containing
  the intended nix store paths of each output.  Each output path
  is a directory in nix store whose name is a concatenation of the
  cryptographic hash of all build inputs, the name attribute and the
  output name. The output directories are created before the build
  starts, environment variables for each output name are passed to the
  build script.  The build script stores its output artifacts at those
  paths.

Other environment variables:

* ``NIX_BUILD_TOP``: path of the temporary directory for this build.
* ``NIX_STORE``: the top-level Nix store directory (typically, /nix/store).

These are set to prevent issues when they are not set:

* ``TMPDIR``, ``TEMPDIR``, ``TMP``, ``TEMP``=``$NIX_BUILD_TOP``
* ``PATH=/path-not-set``
* ``HOME=/homeless-shelter``

The builder is executed as follows:

* cd $TMPDIR/<tmp dir>/
* Clear the environment and set to the attributes as above
* If an output path already exists, it is removed
* The builder is executed with the arguments specified by the attribute args.
* If the builder exits with exit code 0, it is considered to have succeeded.
* A log of standard output and error is written to ``/nix/var/log/nix``

Post build:

* The temporary directory is removed (unless the -K option was specified).
* If the build was successful, Nix scans each output path for references
  to input paths by looking for the hash parts of the input paths. Since
  these are potential runtime dependencies, Nix registers them as
  dependencies of the output paths.

stdenv and lib
~~~~~~~~~~~~~~

Attribute path ``nixpkgs.lib`` contains `a library of functions
<https://nixos.org/nixpkgs/manual/#chap-functions>`_ to help in
writing package definitions.  Attribute path `nixpkgs.stdenv
<https://nixos.org/nixpkgs/manual/#chap-stdenv>`_ contains a nix package that
provides a standard build environment including gcc, GNU coreutils, GNU
findutils and other basic tools::

    $ nix-env -qaP -A nixpkgs.stdenv
    nixpkgs.stdenv  stdenv-darwin

mkDerivation
~~~~~~~~~~~~

``stdenv`` provides a wrapper around `builtins.derivation
<https://nixos.org/nix/manual/#ssec-derivation>`_
called `stdenv.mkDerivation
<https://nixos.org/nixpkgs/manual/#sec-using-stdenv>`_.
It adds a default value for ``system`` and always uses ``bash`` as the
``builder``, to which the supplied builder is passed as a command-line
argument::

  stdenv.mkDerivation {
    name    # name of the package, if pname and version are specified this is
            # automatically set to "${pname}-${version}"
    pname   # package name
    version # package version
    src     # source directory containing the package source
    builder ? # use your own builder script instead of genericBuild
    buildInputs ? # dependencies e.g. [libbar perl ncurses]
    buildPhase ? # build phase script
    installPhase ? # install phase script
    ...
  }

Environment of the builder: In addition to the environment provided by
``derivation``:

* ``stdenv`` contains the path to ``stdenv`` package. The shell script ``$stdenv/setup`` is
  typically sourced by the builder script to setup the ``stdenv`` environment.
* ``buildInputs`` attribute ensures that the bin subdirectories of these
  packages appear in the ``PATH`` environment variable during the build,
  that their include subdirectories are searched by the C compiler, and so
  on.

Builder script execution:

* If ``builder`` is not set, then the ``genericBuild`` function from
  ``$stdenv/setup`` is called as build script. ``buildPhase``, ``installPhase``
  customizations in ``mkDerivation`` are used by ``genericBuild`` allowing
  customization of its behavior. `See the manual
  <https://nixos.org/nixpkgs/manual/#sec-stdenv-phases>`_ to check out
  more details about the build phases.
* If ``builder`` is set then the specified builder script is invoked with
  ``bash``. You can source ``$stdenv/setup`` in the script. You can still
  define ``buildPhase``, ``installPhase`` etc as shell functions and then
  invoke ``genericBuild`` in your script.

To checkout the shell functions and environments available in ``$stdenv/setup``
install ``stdenv`` and visit its store path.
The source of ``mkDerivation`` can be found in
``$HOME/.nix-defexpr/channels/nixpkgs/pkgs/stdenv/generic/make-derivation.nix``.

Quick References
----------------

* https://nixos.wiki/wiki/Nix_Expression_Language
* https://nixcloud.io/tour/ A tour of Nix (language)
* https://medium.com/@MrJamesFisher/nix-by-example-a0063a1a4c55 Nix by example
* https://nix.dev/anti-patterns/language.html
