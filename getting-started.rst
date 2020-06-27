Getting Started with Haskell
============================

The purpose of this guide is to document best practices for new users to
get started on using the Haskell compiler ``ghc`` and Haskell build tool
``cabal``. If you follow this guide and run into a problem `please raise
an issue here <https://github.com/composewell/haskell-dev/issues/new>`_.
For diagnostics, please see the FAQ_ section in the end.

This guide is primarily oriented towards POSIX shell users.  On
Windows, ``ghc`` is installed on top of ``msys`` which provides a POSIX
shell. This document, with some changes, may apply to Windows as well
but it has not been tested.  We assume ``cabal`` version 3.0 or higher
and GHC version ``8.2.1`` or higher.

.. contents:: Table of Contents
   :depth: 1

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

Haskell Packages
----------------

The canonical Haskell package repository is `Hackage
<http://hackage.haskell.org/>`_, hosting thousands of packages consisting of
libraries as well as useful executable programs.  You can browse the packages
and their documentation on `Hackage <http://hackage.haskell.org/>`_.

``cabal`` can install packages from Hackage so that they can be used by
``ghc``. To get familiar with ``cabal``'s commands and options use::

    $ cabal --help

Before you can install or use the packages, you need to fetch and update
the list of packages from Hackage::

    $ cabal update

Side Note: ``cabal`` keeps its housekeeping data in ``$HOME/.cabal``. The
fetched package index and packages are kept in
``$HOME/.cabal/packages/hackage.haskell.org/``.

Compiling Haskell Programs
--------------------------

Isolated Compilation
~~~~~~~~~~~~~~~~~~~~

We compiled the above program using ``ghc`` directly. This program
has a dependency only on the ``base`` package which is shipped with ``ghc``.
In general, a program may depend on several other packages and each
dependency may have many versions. When you are compiling program ``A``
it may require package ``X`` version ``v1``, on the other hand when
you are compiling program ``B`` it may require package ``X`` version
``v2``. If we install packages globally then we can only install/activate
one version of any given package. This means we can either compile
program ``A`` or program ``B`` but not both together.

To avoid such issues, we recommend that you ALWAYS use a dedicated
package directory to build a program, even if it is a single file
program. Using a dedicated directory provides an isolated build
environment (see details in the following sections). ``cabal`` would
install the necessary dependency versions specific to the program and
invoke ``ghc`` with those dependencies versions. This way each program
gets its own build environment and they do not interfere with each
other.

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

Building and Running a Program
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let us write the hello world example in an isolated build
environment. We will create a cabal ``package`` for our program to
do so.  First create a directory for our ``hello-world`` package. This
directory will contain an independent and isolated build environment for
our package::

    $ mkdir hello-world
    $ cd hello-world

Now create a package description file (``<package name>.cabal``). This
file contains important information on how to build the package,
including dependencies of the package, compiler options, executables,
benchmarks, test suites to build::

    $ cabal init

This would create a file named ``hello-world.cabal`` in the current
directory. The contents of the file look like this::

  name:                hello-world
  version:             0.1.0.0

  executable hello-world
    main-is:             Main.hs
    build-depends:       base >=4.13 && <4.14

It says, this directory contains a package named ``hello-world``
whose version number is ``0.1.0.0``. The package contains an
executable called ``hello-world`` whose main module lives in the
file ``Main.hs``.  The package depends on the ``base`` package.
`base <http://hackage.haskell.org/package/base>`_ is a fundamental
library package required by all Haskell programs. ``base`` package
provides the `Prelude` module which is implicitly imported by Haskell
programs. The function ``putStrLn`` in our program comes from the
`Prelude <http://hackage.haskell.org/package/base/docs/Prelude.html>`_
module. You can add more packages here separated with commas.

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

If we want to just build the package and not run it::

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
~~~~~~~~~~~~~~~~~~~~~~~~~~

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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now that we have an isolated package build setup. We can even directly use
``ghc`` (version ``8.2.1`` or higher) to compile the files in our package
instead of using ``cabal build``.

For ``ghc`` to use the same package dependencies as ``cabal`` invokes
it with we need to first create an ``environment`` file for ``ghc`` to
use::

  cabal build --write-ghc-environment-files=always

This will generate an ``environment`` file at the root of the package
directory::

  $ ls .ghc.*
  .ghc.environment.x86_64-darwin-8.8.3

You can put this in your ``$HOME/cabal.config`` or ``cabal.project.local`` so
that you do not have to specify this on each build::

    $ cat cabal.project.local
    write-ghc-environment-files: always

Now we can use ``ghc`` directly to compile any module in this package::

  $ ghc Main.hs
  Loaded package environment from /Users/harendra/hello-world/.ghc.environment.x86_64-darwin-8.8.3
  [1 of 1] Compiling Main             ( Main.hs, Main.o )
  Linking Main ...

  $ ./Main
  Hello, Haskell!

How It works?
.............

From version ``8.2.1`` onwards ``ghc`` always looks for an environment
file in the current directory or in any of the parent directories
and loads it if found. The environment file contains a list of package
databases and packages for use by ``ghc``.

``cabal build`` sets up the environment file to use the package
dependency versions that it has selected for the current package.  

Note: Do not forget to do a ``cabal build`` before compiling with ``ghc``
directly.

Using extra dependencies
........................

If you want to use a package not specified in the ``build-depends``
section of the cabal file then you need to first install it from within
the project directory and then explicitly ask ``ghc`` to use it::

    $ cabal install unordered-containers
    $ ghc -package unordered-containers Main.hs

GHC Documentation
~~~~~~~~~~~~~~~~~

It may be a good idea to go through the `ghc` help text::

    $ ghc --help
    $ man ghc

See `the GHC user guide <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/>`_ for more details.

Creating Modules
~~~~~~~~~~~~~~~~

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

Using Dependencies
------------------

Using Packages from Hackage
~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

Using Packages from github
~~~~~~~~~~~~~~~~~~~~~~~~~~

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

Using Non-Haskell Dependencies
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When a package depends on a C library we need to tell cabal where the
library and its header files are::

  $ cat cabal.project.local
  package text-icu
    extra-include-dirs: /opt/local/include
    extra-lib-dirs: /opt/local/lib

NOTE: It seems this works only in cabal.project.local and not in cabal.project,
see https://github.com/haskell/cabal/issues/2997 .

We can also use the command line options, however, they do not apply to
dependencies, they only apply to local packages::

    $ cabal build --extra-include-dirs=/opt/local/include --extra-lib-dirs=/opt/local/lib 
    
GHC uses ``gcc`` or ``clang`` to compile C sources. The header file search
path and library search path for ``gcc`` and ``clang`` can be specified using
environment variables.  This could be useful when you are not in a
project context e.g. when installing a package using ``cabal install``
or if some other program is invoking cabal from inside::

  $ export C_INCLUDE_PATH=/opt/local/include
  $ export CPLUS_INCLUDE_PATH=/opt/local/include
  $ export LIBRARY_PATH=/opt/local/lib:/usr/lib:/lib

Customizing how dependencies are built
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Options passed to the build command, are ``global`` which means they
apply to all your source packages, their dependencies, and dependencies
of dependencies. For example::

    $ cabal build --ghc-options=-Werror

We can use the ``configure`` command to persistently save the settings in a
``cabal.project.local`` file::

  $ cabal configure --ghc-options=-Werror 
  $ cat cabal.project.local

  package *
    ghc-options: -Werror

  program-options
    ghc-options: -Werror

If we want a setting to be applied only to a certain package or dependency::

  $ cat cabal.project
  package streamly
    ghc-options: -Werror

Freezing Dependency Versions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``cabal`` picks the dependency versions based on the constraints
specified in the cabal file. When newer versions of dependencies become
available or if the compiler version changes (which changes the ``base``
package version), cabal's dependency solver can pick a different set of
dependency versions satisfying the constraints. However, if you want to
freeze the versions picked by ``cabal`` you can use the ``cabal freeze``
command. It generates a ``cabal.project.freeze`` file consisting of the
exact versions and build flags of the packages chosen by cabal. If that
file exists ``cabal`` always picks up exactly those versions.

This command can also be useful if you want to know all the dependencies of the
project and their versions.

Using Stackage Snapshots
~~~~~~~~~~~~~~~~~~~~~~~~

`Stackage <https://www.stackage.org/>`_ releases a consistent set
of versions of Haskell packages that are known to build together,
known as stackage ``lts`` Haskell snapshots. You can use the ``lts``
snapshots with cabal using the ``cabal.project.freeze`` file provided by
stackage::

    curl https://www.stackage.org/lts/cabal.config > cabal.project.freeze

Packages Tied to GHC
~~~~~~~~~~~~~~~~~~~~

There are some packages whose versions change along with GHC versions
because they depend on the GHC version. Versions of these packages (in
the dependency version ranges) cannot be upgraded unless you use an
appropriate version of GHC as well.  These packages are also known as
wired-in packages in ghc. Some important wired-in packages are:

* `base <http://hackage.haskell.org/package/base>`_
* `template-haskell <http://hackage.haskell.org/package/template-haskell>`_
* `ghc-prim <http://hackage.haskell.org/package/ghc-prim>`_

`See this link for a complete list of wired-in packages
<https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/libraries>`_.

Cabal configuration
~~~~~~~~~~~~~~~~~~~

The behavior of ``cabal`` is determined by the following configuration,
in the increasing priority order:

* $HOME/.cabal/config (the user-wide global configuration)
* cabal.project (the project configuration)
* cabal.project.freeze (the output of cabal freeze)
* cabal.project.local (the output of cabal configure)
* command line flags
* Environment variables

`See cabal.project section in cabal user guide <https://www.haskell.org/cabal/users-guide/nix-local-build.html#configuring-builds-with-cabal-project>`_.

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

Haskell (GHC) versions
----------------------

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
``8.8.3``.

IMPORTANT NOTE: The activated version of ``ghc`` changes in all
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

Using Hoogle search on local package
------------------------------------

Generate a hoogle input file for generating a hoogle database::

    $ cabal haddock --haddock-hoogle
    ...
    ~/streamly/dist-newstyle/build/x86_64-osx/ghc-8.8.3/streamly-0.7.2/doc/html/streamly/streamly.txt

Generate a hoogle database from the directory printed by the command above::

    $ hoogle generate --local=~/streamly/dist-newstyle/build/x86_64-osx/ghc-8.8.3/streamly-0.7.2/doc/html/streamly/
    $ ls -al ~/.hoogle/*.hoo
    -rw-r--r--  1 harendra  staff  913433 Jun 18 21:05 /Users/harendra/.hoogle/default-haskell-5.0.17.hoo

Run hoogle server::

    $ hoogle server --local

``--local`` is important to allow following the ``file://`` links. Visit
``http://127.0.0.1:8080/`` in your browser.

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

.. _FAQ:

Frequently Asked Questions (FAQ)
--------------------------------

Make sure that you have read and followed the guide above.

When building your project
~~~~~~~~~~~~~~~~~~~~~~~~~~

Q: I am getting a ``Could not find module ...`` error::

  Main.hs:3:1: error:
  Could not find module ‘Data.Foo’
  Perhaps you meant
    Data.Bool (from base-4.13.0.0)
    Data.Fix (needs flag -package-key data-fix-0.2.1)
    Data.Pool (needs flag -package-key resource-pool-0.2.3.2)
  Use -v (or `:set -v` in ghci) to see a list of the files searched for.
    |
  3 | import Data.Foo
    | ^^^^^^^^^^^^^^^

A: You have used the module ``Data.Foo`` in your program but you have not
specified the package providing this module in the ``build-depends`` field of
your executable or library section of your cabal file. Add it by editing the
``.cabal`` file. Assuming the module ``Data.Foo`` is in package ``foo``::

  executable hello-world
    main-is:             Main.hs
    build-depends:       base >=4.13 && <4.14, foo

If you do not know which package the module ``Data.Foo`` belongs to, you
can search the module name on `hoogle <https://hoogle.haskell.org/>`_ or use
the `documentation by module on stackage
<https://www.stackage.org/lts/docs>`_

Q: I am getting a ``Could not resolve dependencies`` along with
``constraint from non-upgradeable package requires installed instance``
error::

  Resolving dependencies...
  cabal: Could not resolve dependencies:
  [__0] trying: clock-project-0.1.0.0 (user goal)
  [__1] next goal: base (dependency of clock-project)
  [__1] rejecting: base-4.13.0.0/installed-4.13.0.0 (conflict: clock-project =>
  base>=4.14 && <4.15)
  [__1] rejecting: base-4.14.0.0, base-4.13.0.0, base-4.12.0.0, base-4.11.1.0,
  base-4.11.0.0, base-4.10.1.0, base-4.10.0.0, base-4.9.1.0, base-4.9.0.0,
  base-4.8.2.0, base-4.8.1.0, base-4.8.0.0, base-4.7.0.2, base-4.7.0.1,
  base-4.7.0.0, base-4.6.0.1, base-4.6.0.0, base-4.5.1.0, base-4.5.0.0,
  base-4.4.1.0, base-4.4.0.0, base-4.3.1.0, base-4.3.0.0, base-4.2.0.2,
  base-4.2.0.1, base-4.2.0.0, base-4.1.0.0, base-4.0.0.0, base-3.0.3.2,
  base-3.0.3.1 (constraint from non-upgradeable package requires installed
  instance)
  [__1] fail (backjumping, conflict set: base, clock-project)
  After searching the rest of the dependency tree exhaustively, these were the
  goals I've had most trouble fulfilling: base, clock-project

A. The key part is ``conflict: clock-project => base>=4.14 && <4.15``
and ``constraint from non-upgradeable package requires installed
instance``. The ``base`` package version constraints in your cabal file are in
conflict with the ``base`` version of the current compiler you are using. Each
compiler version is tied to a particular ``base`` package version which cannot
be upgraded. You need to either change the constraints in your cabal file to
allow the ``base`` package version corresponding to the ``ghc`` version you are
using or change your ``ghc`` version.

You can see ``base`` versions corresponding to ``ghc`` versions `here
<https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/libraries/version-history>`_
. If you are using ``ghcup``, ``ghcup list`` shows ``ghc`` and
corresponding ``base`` versions.

Q: I am getting a ``Could not resolve dependencies`` error but I am not using
the packages mentioned in the error message in my project::

  Resolving dependencies...
  cabal: Could not resolve dependencies:
  [__0] trying: slides-0.1.0.0 (user goal)
  [__1] trying: base-4.13.0.0/installed-4.13.0.0 (dependency of slides)
  ...

A: You may not be using the dependency package in question but
it may be a dependency of a dependency. In such case, you cannot
fix the dependency version in your cabal file but you can use the
``--allow-newer`` ``cabal`` option e.g. ``cabal build --allow-newer
...``. You can also allow newer version of a specific set of packages
e.g. ``cabal build --allow-newer=streamly ...``.

Q: I do not see any dependency version issue in my ``.cabal`` file, but
I am still getting a ``Could not resolve dependencies`` error. I am
puzzled::

    Resolving dependencies...
    cabal: Could not resolve dependencies:
    [__0] next goal: xls (user goal)
    [__0] rejecting: xls-0.1.3, xls-0.1.2, xls-0.1.1 (constraint from user target
    requires ==0.1.0)
    [__0] rejecting: xls-0.1.0 (constraint from user target requires ==0.1.3)
    [__0] fail (backjumping, conflict set: xls)
    After searching the rest of the dependency tree exhaustively, these were the
    goals I've had most trouble fulfilling: xls

A: ``cabal`` looks for ``.cabal`` files in all the subdirectories. If
the ``.cabal`` file in your current directory seems fine, look for any other
``.cabal`` files in your tree (which may be lying around by mistake) 

Q: Some random weird problem, unexpected behavior when building a project:

A: When all else fails, try ``cabal clean`` or removing the ``dist-newstyle``
directory.

Q: I am getting these strange messages::

    $ cabal test
    cabal: Cannot test the package streamly-process-0.1.0.0 because none of the
    components are available to build: the test suite 'system-process-test' is not
    available because the solver did not find a plan that included the test
    suites. Force the solver to enable this for all packages by adding the line
    'tests: True' to the 'cabal.project.local' file.

    $ cabal test system-process-test
    cabal: Cannot run the test suite 'system-process-test' because the solver did
    not find a plan that included the test suites for streamly-process-0.1.0.0. It
    is probably worth trying again with test suites explicitly enabled in the
    configuration in the cabal.project{.local} file. This will ask the solver to
    find a plan with the test suites available. It will either fail with an
    explanation or find a different plan that uses different versions of some
    other packages. Use the '--dry-run' flag to see package versions and check
    that you are happy with the choices.

A: Use ``cabal test --enable-test`` and you will get a better error message.

When Compiling Directly With GHC or using GHCi
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Q: ``cannot satisfy -package-id`` error::

   $ ghc -O2 zz.hs
   Loaded package environment from /projects/streamly/.ghc.environment.x86_64-darwin-8.8.3
   <command line>: cannot satisfy -package-id fusion-plugin-0.2.1-inplace
       (use -v for more information)

A: The package ``fusion-plugin-0.2.1`` is specified as a dependency in
your cabal file.  This package is listed in the ``.ghc.environment*``
file but has not been built. ``-inplace`` means it is a local package
and not one downloaded from Hackage. Run ``cabal build fusion-plugin``
to make this error go away.

Q: ``Could not find module`` error::

  examples/WordClassifier.hs:28:1: error:
      Could not find module ‘Data.None’
      Use -v (or `:set -v` in ghci) to see a list of the files searched for.
     |
  28 | import Data.None
     | ^^^^^^^^^^^^^^^^

A: To resolve this:

1) Add the package providing module ``Data.None`` in the
   ``build-depends`` field in cabal file. Do not forget to do ``cabal build
   --write-ghc-environment-files=always`` after adding it.

   Alternatively, use ``cabal install <package>`` (from within the project
   directory) to add the package to your ``.ghc.environment.*`` file.

2) If the package providing ``Data.None`` is already present in
   ``build-depends``, check if you have a ``.ghc.environment.*`` file in the
   project directory, if not use ``cabal build
   --write-ghc-environment-files=always`` to generate it.

Q: ``Could not load module ... It is a member of the hidden package`` error::

  examples/WordClassifier.hs:13:1: error:
  Could not load module ‘Data.HashMap.Strict’
  It is a member of the hidden package ‘unordered-containers-0.2.10.0’.
  You can run ‘:set -package unordered-containers’ to expose it.
  (Note: this unloads all the modules in the current scope.)
  Use -v (or `:set -v` in ghci) to see a list of the files searched for.
     |
  13 | import qualified Data.HashMap.Strict as Map
     | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A: The package providing the module ``Data.HashMap.Strict``, i.e.
``unordered-containers`` is available in ``cabal``'s package cache,
but it is not mentioned as a dependency in your project. Do any of the
following to resolve this:

1) Add ``unordered-containers`` in the ``build-depends`` field in cabal file.
   Do not forget to do ``cabal build --write-ghc-environment-files=always``
   after adding it.

2) Use ``cabal install <package>`` (from within the project directory) to add
   the package to your ``.ghc.environment.*`` file.

3) Use ``ghc -package unordered-containers`` to make it available to ``ghc``
   anyway.

When Installing packages
~~~~~~~~~~~~~~~~~~~~~~~~

Occasionally you may install *library* packages *within your project*
scope. However, when installing *executable* packages, make sure that you are
outside a project directory, otherwise the project's dependency
constraints would apply to the package you are installing and the
installation may fail. ``cabal install`` may fail with some of the
errors described above, see the sections above for a resolution of
those.

Occasionally ``cabal install foo`` may fail with a compilation error due to
several reasons:

* In many cases, packages take time to move to newer versions of
  ``ghc``.  Ideally, if the dependency version bounds are correctly set
  then the dependency resolution itself should fail with a newer compiler.
  However, many package authors use relaxed upper bounds on dependencies,
  and the build may fail with a compilation error if breaking changes
  arrive in a newer version.
* There may be an error in specifying the version bounds. The version bounds
  may not have been tested.

You can resolve these errors by:

* Try the ``--allow-newer`` cabal option mentioned earlier.
* Go to the Hackage page of the package, go to the ``Status`` section and click
  on ``Hackage CI`` button, you can see the build matrix of the package.
  From the matrix you can find out which compiler versions can compile
  this package.  You can also take a look at the ``tested-with`` field
  in the cabal file of the package, to find the right compiler version
  to use. Try an appropriate version of ``ghc``.

Bugs
~~~~

Q: ``ghc-pkg list`` does not show all the packages that cabal can use.

A: Ideally, once we write the ``.ghc.environment`` file, ``ghc-pkg``
should be able to list all the packages that cabal uses in a
project. However, ``ghc-pkg`` is not (yet?) aware of the environment
files and it lists only packages that are directly registered with ``ghc``.
This would be a minimal set when using latest cabal workflows, cabal does not
register packages directly with ``ghc`` it uses environment files.

Q: ``cabal`` throws an error like this, even though I have cabal-version as the
first line::

    Errors encountered when parsing cabal file ./xls.cabal:

    xls.cabal:1:1: error:
    cabal-version should be at the beginning of the file starting with spec version 2.2. See https://github.com/haskell/cabal/issues/4899

        1 | cabal-version: 3
          | ^

A: This is a bug in cabal, use "3.0" instead of "3" in version.

Q: Can ``cabal`` report better errors?

A: Yes.

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
   * `Cabal handy reference <https://www.haskell.org/cabal/users-guide/cabal-projectindex.html>`_

Package Repositories, Documentation, Search:

* `Haskell package repository (Hackage) <http://hackage.haskell.org/>`_
* `Stackage package snapshots <https://www.stackage.org/>`_
* `Documentation by module on stackage <https://www.stackage.org/lts/docs>`_
* `Haskell Search Engine <https://hoogle.haskell.org/>`_

Packages:

* `base: The Haskell standard library <http://hackage.haskell.org/package/base>`_

  * `Haskell Debug.Trace module <hackage.haskell.org/package/base/docs/Debug-Trace.html>`_
  * `Haskell Prelude module <http://hackage.haskell.org/package/base/docs/Prelude.html>`_
* `template-haskell: The Haskell macro system <http://hackage.haskell.org/package/template-haskell>`_
* `ghc-prim: Primitives provided by GHC <http://hackage.haskell.org/package/ghc-prim>`_
* `GHC boot packages <https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/libraries>`_.
* `GHC version to GHC boot package version mapping <https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/libraries/version-history>`_
