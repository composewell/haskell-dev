Getting Started with Haskell
============================

The purpose of this guide is to document best practices for new users to
get started on using the Haskell compiler ``ghc`` and Haskell build tool
``cabal``. If you follow this guide and run into a problem `please raise
an issue here <https://github.com/composewell/haskell-dev/issues/new>`_.
For diagnostics, please see the FAQ section in the end.

This guide is primarily oriented towards POSIX shell users.  On
Windows, ``ghc`` is installed on top of ``msys`` which provides a POSIX
shell. This document, with some changes, may apply to Windows as well
but it has not been tested.  We assume ``cabal`` version 3.0 or higher
and GHC version ``8.2.1`` or higher.

Install ``ghc`` and ``cabal``
-----------------------------

Install the Glasgow Haskell compiler ``ghc`` and the build tool ``cabal``.

* `Preferred method for Linux and Mac OSX <install/ghcup.rst>`_
* `Shell method for Linux/Mac OSX/POSIX <install/posix-via-shell.rst>`_
* `Other methods <install/other.rst>`_

Verify Installation
~~~~~~~~~~~~~~~~~~~

To check that the tools are installed and available in your PATH, run::

    $ ghc --version
    $ ghc -e 'putStrLn "hello world!"'
    $ cabal --version

If it does not work check if the directory where the tools are installed
is in your `PATH`. Make sure you have followed the installation
instructions carefully, especially the part where it asks you to setup
the `PATH` for you.

Editors
-------

``vim``, ``emacs`` and ``vscode`` are popular editors having reasonable
support for the Haskell tool chain.

Hello World!
------------

An executable must have a ``Main`` module consisting of a `main`
function where the program execution starts::

  $ cat hello.hs
  module Main where
  main :: IO ()
  main = putStrLn "hello world"

  $ ghc hello.hs
  $ ./hello 

The first line is optional, any module without an explicit name is
treated as a ``Main`` module by default.

Isolated Compilation
--------------------

We compiled the above program using ``ghc`` directly. This program
has a dependency only on the ``base`` package which is shipped with ``ghc``.
In general, a program may depend on several other packages and each
dependency may have many versions. When you are compiling program ``A``
it may require package ``X`` version ``v1``, when you are compiling
program ``B`` it may require package ``X`` version ``v2``. When
installing packages globally we can only install/activate one version
of a package, so we can either compile program ``A`` or program ``B``
but not both together.

To avoid such issues, we recommend that you ALWAYS use a separate
``cabal`` package directory to build a program, it provides an isolated
build environment (see the following sections), even if it is a single
file program. ``cabal`` would install the necessary dependency versions
specific to the program and invoke ``ghc`` with those dependencies
versions, not interfering with any other program.

When installing library packages globally, there are many other ghc
package management related details that you may need to know to debug
issues around ``ghc`` not being able to use a package or cabal not being
able to install a package due to version conflicts.  Therefore, we
recommend that you NEVER use ``ghc`` to compile directly outside a cabal
build directory. This also means that you never need to use ``cabal
install`` to install library packages outside of a cabal directory. You
can of course use ``cabal install`` to install executables globally e.g.
``cabal install hlint --install-dir ~/.local/bin`` would install the
``hlint`` executable in ``~/.local/bin``.

Haskell Packages
----------------

The canonical Haskell package repository is `Hackage
<http://hackage.haskell.org/>`_ hosting thousands of packages consisting of
libraries as well as useful executable programs.  You can browse the packages
and their documentation on `Hackage <http://hackage.haskell.org/>`_.

``cabal`` can install packages from Hackage so that they can be used by
``ghc``. Check out ``cabal`` help::

    $ cabal --help

Before you can install or use them, you need to fetch and update the index of
packages from Hackage::

    $ cabal update

Note: ``cabal`` keeps its housekeeping data in ``$HOME/.cabal``. The
fetched package index and packages are kept in
``$HOME/.cabal/packages/hackage.haskell.org/``.

Building a Program
------------------

Let us write the hello world example in an isolated build environment.
``cabal`` always builds packages. First create a directory for our
``hello-world`` package. This directory will contain an independent and
isolated build environment for our package::

    $ mkdir hello-world
    $ cd hello-world

Now create a package description file (``<package>.cabal``). The
``.cabal`` file contains important information on how to build the
package, including dependencies of the package, compiler options,
executables, benchmarks, test suites to build::

    $ cabal init

This would create a file named ``hello-world.cabal`` in the current
directory. The contents of the file look like this::

  name:                hello-world
  version:             0.1.0.0

  executable hello-world
    main-is:             Main.hs
    build-depends:       base >=4.13 && <4.14

It says, this directory contains a package named ``hello-world``
whose version number is ``0.1.0.0``. The package would build an
executable called ``hello-world`` whose main module lives in the
file ``Main.hs``.  The package depends on the ``base`` package.
`base <http://hackage.haskell.org/package/base>`_ is a fundamental
library package required by all Haskell programs. ``base`` package
provides the `Prelude` module which is implicitly imported by Haskell
programs. The function ``putStrLn`` in our program comes from the
`Prelude <http://hackage.haskell.org/package/base/docs/Prelude.html>`_
module.

The default package name ``hello-world`` is automatically derived by
``cabal init`` from the current directory name.  You can use a different
name using ``cabal init -p``. Or you can just edit the ``.cabal``
file and change the package name field, you have to remember that the
``.cabal`` file name must always be the same as the package name so if
you change the package name you would have to rename it as well.  You
can use ``cabal init --help`` to know about more ``init`` options to use.

We can now write our program in the file ``Main.hs``. In fact, ``cabal
init`` itself creates one for us, we can edit it if we want::

  $ cat Main.hs
  module Main where

  main :: IO ()
  main = putStrLn "Hello, Haskell!"

Note that ``Main.hs`` is not a special name, you can change it to
whatever name you want as long as you use the same name in the ``main-is``
field of the ``executable`` section in the ``.cabal`` file

Let us now build and run our program::

  $ cabal run

This command builds the executable ``hello-world`` from the module ``Main.hs``
as specified in the ``.cabal`` file, and then runs the executable. The
executable and all other intermediate build artifacts are created in the
``dist-newstyle`` directory.

We can clean the build artifacts using::

  $ cabal clean

If we want to just build and not run::

  $ cabal build

The executable ``hello-world`` can be found inside the ``dist-newstyle``
directory. ``cabal build -v`` would print its path as well as a lot of other
information including how it invokes ``ghc``::

    $ cabal build -v
    ...
    Linking /Users/harendra/hello-world/dist-newstyle/build/x86_64-osx/ghc-8.8.3/hello-world-0.1.0.0/x/hello-world/build/hello-world/hello-world ...

We can run that executable directly too instead of using ``cabal run``::

    $ /Users/harendra/hello-world/dist-newstyle/build/x86_64-osx/ghc-8.8.3/hello-world-0.1.0.0/x/hello-world/build/hello-world/hello-world
    Hello, Haskell!

Use ``cabal --help`` for general ``cabal`` commands and options. For
more details on command line options please refer to `this section in
cabal user guide <https://www.haskell.org/cabal/users-guide/nix-local-build.html>`_.
To know more about the fields you can use in the cabal file `please see this
section <https://www.haskell.org/cabal/users-guide/developing-packages.html#package-descriptions>`_.

Note: Command line options and their behavior has changed in recent versions
of ``cabal`` and the newer options (with a ``v2-`` prefix) are now used
by default in cabal 3.0 or higher (i.e. ``cabal build`` is the same as
``cabal v2-build``). Please do not get confused with the older cabal
command line options (with a ``v1-`` prefix) which may be mentioned in
some sections of the user guide. 

Specifying ``ghc-options``
--------------------------

In the ``executable`` stanza of the cabal file we can use the ``ghc-options``
field to pass command line options to ``ghc`` when building our executable::

  executable hello-world
    main-is:             Main.hs
    build-depends:       base >=4.13 && <4.14
    ghc-options:         -v

If you are interested in how things work internally, the ``ghc -v``
option could be especially useful to see how cabal sets up the package
databases for ``ghc``, i.e. where the compiled dependencies are coming from::

  Using binary package database: /Users/harendra/.ghcup/ghc/8.8.3/lib/ghc-8.8.3/package.conf.d/package.cache
  Using binary package database: /Users/harendra/.cabal/store/ghc-8.8.3/package.db/package.cache
  Using binary package database: /Users/harendra/hello-world/dist-newstyle/packagedb/ghc-8.8.3/package.cache
  Using binary package database: /Users/harendra/hello-world/dist-newstyle/build/x86_64-osx/ghc-8.8.3/hello-world-0.1.0.0/x/hello-world/package.conf.inplace/package.cache

This could be useful if you face an issue where ``ghc`` complains that a
particular package is not found.  For more details about how ghc package
management works see `GHC package management guide <ghc-packages.md>`_.

Compiling with ``ghc`` directly
-------------------------------

Now that we have an isolated package build setup. We can even directly use
``ghc`` (version ``8.2.1`` or higher) to compile the files in our package
instead of using ``cabal build``.

For ``ghc`` to use the same package dependencies as ``cabal`` invokes
it with we need to first create an ``environment`` file for ``ghc`` to
use::

  cabal build --write-ghc-environment-files=always

This will generate an ``environment`` file at the root of the package
directory, and also configure `cabal` to produce one on each ``cabal
build`` ::

  $ ls .ghc.*
  .ghc.environment.x86_64-darwin-8.8.3

Now we can use ``ghc`` directly to compile any module in this package::

  $ ghc Main.hs
  Loaded package environment from /Users/harendra/hello-world/.ghc.environment.x86_64-darwin-8.8.3
  [1 of 1] Compiling Main             ( Main.hs, Main.o )
  Linking Main ...

  $ ./Main
  Hello, Haskell!

How It works?
~~~~~~~~~~~~~

From version ``8.2.1`` onwards ``ghc`` always looks for an environment
file in the current directory or in any of the parent directories
and loads it if found. The environment file contains a list of package
databases and packages for use by ``ghc``.

``cabal build`` sets up the environment file to use the package
dependency versions that it has selected for the current package.  

Note: Do not forget to do a ``cabal build`` before compiling with ``ghc``
directly.

Using extra dependencies
~~~~~~~~~~~~~~~~~~~~~~~~

If you want to use a package that is not in the ``build-depends`` section of
the cabal file then you need to first install it from within the project
directory and then explicitly ask ``ghc`` to use it::

    $ cabal install unordered-containers
    $ ghc -package unordered-containers Main.hs

GHC Documentation
-----------------

It may be a good idea to go through the `ghc` help text::

    $ ghc --help
    $ man ghc

See `the GHC user guide <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/>`_ for more details.

Modules
-------

Till now, we used only one module the ``Main`` module in our program. Let us
now create another module and import it in our ``Main`` module::

  $ cat Hello.hs
  module Hello (hello) where

  hello :: String
  hello = "Hello World!"

The first line defines the module ``Hello`` and exports the definition
``hello`` to be imported by other modules. Let us now use this definition in
our ``Main`` module::

  $ cat Main.hs
  module Main where

  import Hello (hello)

  main :: IO ()
  main = putStrLn hello

Now we can run it::

    $ cabal run
    Hello World!

We can see that it compiles and runs but produces the following warning::

  <no location info>: warning: [-Wmissing-home-modules]
      These modules are needed for compilation but not listed in your .cabal file's other-modules: 
          Hello

This will go away if we specify the new module in our ``executable``
stanza in the ``.cabal`` file::

  executable hello-world
    main-is:             Main.hs
    other-modules:       Hello
    build-depends:       base >=4.13 && <4.14

We need to keep the following in mind when creating modules:

* Module name (``Hello``) used in the module construct must match its file
  name (``Hello.hs``).
* For hierarchical modules, if the module name is ``Example.Hello``
  then the path of the module in the file system must be
  ``Example/Hello.hs`` relative to the import root.

Using Library Packages
----------------------

We can use any package from Hackage in our program by specifying it in
the ``build-depends`` field (do not forget to execute ``cabal update``
at least once before this).  Let's try to use the library `streamly
<http://hackage.haskell.org/package/streamly>`_ in our program.

First add ``streamly`` to the dependencies::

  executable hello-world
    main-is:             Main.hs
    build-depends:       base >=4.13 && <4.14, streamly

``import`` and use it in our ``Main`` module::

  $ cat Main.hs
  import qualified Streamly.Prelude as S

  main = S.drain $ S.fromListM [putStrLn "hello", putStrLn "world"]

  $ cabal run

See `the README for streamly on Hackage
<http://hackage.haskell.org/package/streamly#readme>`_ for more code snippets
to try out.

Interactive Haskell REPL (GHCi)
-------------------------------

Once you have created an isolated package build environment, you can
use the REPL (read-eval-print-loop) for fast evaluation of Haskell
expressions or modules.

For example, if you want to play with ``streamly``, type the following in your
cabal package directory from the previous section::

    $ cabal repl
    Build profile: -w ghc-8.8.3 -O1
    In order, the following will be built (use -v for more details):
     - hello-world-0.1.0.0 (exe:hello-world) (ephemeral targets)
    Preprocessing executable 'hello-world' for hello-world-0.1.0.0..
    GHCi, version 8.8.3: https://www.haskell.org/ghc/  :? for help
    [1 of 1] Compiling Main             ( Main.hs, interpreted )
    Ok, one module loaded.
    *Main>

It starts ``ghci``, the Haskell REPL, loading the ``Main`` module. You now
have all the imports and symbols from the ``Main`` module accessible in the
repl, you can evaluate those interactively::

    *Main> main
    hello
    world
    *Main> S.drain $ S.mapM print $ S.fromList [1..3]
    1
    2
    3

We have all the dependency packages specified in ``build-depends``
available in GHCi, we can import any modules from those as we wish::

  *Main> import qualified Streamly.Data.Fold as FL
  *Main FL> S.fold (FL.drainBy print) (S.fromList [1..3])
  1
  2
  3

If we want any additional packages to be available in the REPL without
having to specify them in the ``.cabal`` file, we can do that by using a
CLI option::
    
    $ cabal repl --build-depends streamly-bytestring

Like ``ghc``, ``ghci`` also uses the ``environment`` files. Like ``ghc``
we can also use ``ghci`` directly instead of using ``cabal repl`` once
the environment file is generated::

  $ ghci
  GHCi, version 8.8.3: https://www.haskell.org/ghc/  :? for help
  Loaded package environment from /Users/harendra/hello-world/.ghc.environment.x86_64-darwin-8.8.3
  Prelude> :load Main
  [1 of 1] Compiling Main             ( Main.hs, interpreted )
  Ok, one module loaded.
  *Main> main
  hello
  world
  *Main>

Type ``:?`` for help.
See `the GHCi user guide <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html>`_ 
for comprehensive documentation.

Using Packages from github
--------------------------

Let's say you want to play with the latest/unreleased version of `streamly from
github <https://github.com/composewell/streamly>`_. You will need a
``cabal.project`` file to do that. This file describes project level
meta information, for example, all your packages (you can
have multiple packages under the same directory tree, each one as a
subdirectory with a ``.cabal`` file), build options for
each package, where to source the package from etc.::

  $ cat cabal.project
  packages: .
  source-repository-package
    type: git
    location: https://github.com/composewell/streamly
    tag: master

``packages: .`` means include the package in the current directory. The
``source-repository-package`` stanza specifies the ``streamly`` package's
location as a github repository. We can specify any ``commit-id`` in the
``tag`` field.

Now when we build this package, the ``streamly`` package used in the
dependencies will be fetched from the github repository instead of Hackage.
We can now use ``cabal repl`` as usual and we will be using the version of
`streamly` from github::

    $ cabal repl

Debugging
---------

Because of strong type system, there is very little debugging required
in Haskell compared to other languages.  Low level debugging is seldom
required. The most commonly used high level debugging technique is by
printing debug messages on console using
`the Debug.Trace module <hackage.haskell.org/package/base/docs/Debug-Trace.html>`_
or `putStrLn`.

GHCi has a `built in debugger
<https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html#the-ghci-debugger>`_ 
with breakpoint and stepping support, however, this is not used much in
practice. `gdb` can also be used on Haskell executables, however, this is
mainly for advanced users because the low level code has little or no
similarity with the high level code.

Haskell versions
----------------

GHC is the de-facto Haskell compiler, Haskell version practically means
GHC version.  New versions of GHC are released quite often.  Compared
to other languages migrating to newer versions of GHC is pretty
easy. Most packages work for many versions of GHC. However, you can
expect some packages not yet building for the latest version of GHC and some
not supporting versions that are too old. In many cases packages not yet
supprting the newer versions can be built for newer versions by just
using the ``--allow-newer`` option in ``cabal``. The recommended version
range is usually the last three versions.

Selecting the ``ghc`` version to use
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

By default ``cabal`` picks up the ``ghc`` executable available in the
shell ``PATH``.

You can also use the cabal option to use a specific ``ghc`` version e.g.
``cabal build -w ghc-8.8``.

You can also specify the ``ghc`` to be used for compilation in the
``cabal.project`` file using the ``with-compiler`` field.

Selecting the ``ghc`` version with ``ghcup``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``ghcup`` provides multiple versions of ``ghc`` and a currently
activated version. ``ghcup set 8.8.3`` activates the ghc version
``8.8.3``.  Note that the activated version of ``ghc`` changes in all
your shells and not just in the current shell.

``ghcup`` provides ``ghc`` and other version sensitive auxiliary
``executables like ghci``, ``haddock`` etc. in ``$HOME/.ghcup/bin``.

* ``$HOME/.ghcup/bin/ghc`` => currently activated version of ghc
* ``$HOME/.ghcup/bin/ghc-8.8`` => latest ghc-8.8.x
* ``$HOME/.ghcup/bin/ghc-8.8.3`` => ghc-8.8.3

These are symlinks to the binaries in ``$HOME/.ghcup/ghc``. You have the
symlinks available in your shell ``PATH``.  When you use ``ghcup set``
to activate a particular ghc version then it just modifies the ``ghc``
symlink to point to that version.

Build times and Space Utilization
---------------------------------

When we install a package or use a dependency in a program, ``cabal``
fetches the source packages from Hackage and compiles them.  Haskell/GHC
compilation speed is slower than imperative languages, say, C
compilers. A lot of it is because of many expensive optimizations
performed by GHC. In the first few package installs or builds a lot of
dependencies may be fetched and built, therefore, initial builds may
take some time. Please be patient.

However, after the first compilation, ``cabal`` caches and reuses the
previously compiled dependencies across all builds, provided that we
are using the same version of GHC and default compilation options for
dependencies. Whenever you change a compiler version you may see longer
build times due to rebuilding the dependencies for that version. For
faster build speeds avoid changing the compiler version often.

``cabal`` caches the previously built packages in ``$HOME/.cabal`` directory.
The cache size may grow as more dependencies are fetched and built. Commonly
5-10 GB space allocation is reasonable for the cache.

Frequently Asked Questions
--------------------------

Q: When compiling directly with ``ghc``, I get this error::

   $ ghc -O2 zz.hs
   Loaded package environment from /projects/streamly/.ghc.environment.x86_64-darwin-8.8.3
   <command line>: cannot satisfy -package-id fusion-plugin-0.2.1-inplace
       (use -v for more information)

A: package ``fusion-plugin-0.2.1`` is specified as a dependency in the
project but is not built. You can see this package listed in the
``.ghc.environment*`` file. ``-inplace`` means it is a local package and not one
downloaded from Hackage. You can just do ``cabal build fusion-plugin`` to
make this error go away.

Q: When compiling with ``ghc`` directly I get a message like this::

  examples/WordClassifier.hs:28:1: error:
      Could not find module ‘Data.None’
      Use -v (or `:set -v` in ghci) to see a list of the files searched for.
     |
  28 | import Data.None
     | ^^^^^^^^^^^^^^^^

A: You have not included the package providing ``Data.None`` in your
``build-depends`` or you have not executed ``cabal build`` after doing
so. You can use ``cabal install`` (from within the project directory) to
install the package manually.

Q: When compiling with ``ghc`` I get a message like this::

  examples/WordClassifier.hs:13:1: error:
  Could not load module ‘Data.HashMap.Strict’
  It is a member of the hidden package ‘unordered-containers-0.2.10.0’.
  You can run ‘:set -package unordered-containers’ to expose it.
  (Note: this unloads all the modules in the current scope.)
  Use -v (or `:set -v` in ghci) to see a list of the files searched for.
     |
  13 | import qualified Data.HashMap.Strict as Map
     | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A: The package providing the above module is known to cabal,
but it is not in your ``build-depends`` or environment file. Use
``ghc -package unordered-containers`` to make it available to ``ghc``
manually.

Q: ``cabal`` is not able to build or install a package because the package
dependency versions cannot be satisfied::

  Resolving dependencies...
  cabal: Could not resolve dependencies:
  [__0] trying: slides-0.1.0.0 (user goal)
  [__1] trying: base-4.13.0.0/installed-4.13.0.0 (dependency of slides)
  ...

A: Try ``cabal build --allow-newer ...`` or ``cabal install
--allow-newer ...``. You can also allow newer version of a specific set
of packages e.g. ``cabal build --allow-newer=streamly ...``.

Quick References
----------------

Installing:

* `Haskell compiler installer (ghcup) page <https://www.haskell.org/ghcup/>`_
* `Haskell compiler (GHC) download page <https://www.haskell.org/ghc/download.html>`_
* `Haskel build tool (cabal) download page <https://www.haskell.org/cabal/download.html>`_

Tool Guides:

* `GHC user guide <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/>`_
* `Haskell REPL (GHCi) user guide <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html>`_ 
* `GHC package management guide <ghc-packages.md>`_
* `cabal user guide <https://www.haskell.org/cabal/users-guide/>`_
   * `File format and field descriptions <https://www.haskell.org/cabal/users-guide/developing-packages.html>`_
   * `Command line options <https://www.haskell.org/cabal/users-guide/nix-local-build.html>`_

Packages:

* `Haskell package repository (Hackage) <http://hackage.haskell.org/>`_
* `Haskell base package  <http://hackage.haskell.org/package/base>`_
* `Haskell Debug.Trace module <hackage.haskell.org/package/base/docs/Debug-Trace.html>`_
* `Haskell Prelude module <http://hackage.haskell.org/package/base/docs/Prelude.html>`_
