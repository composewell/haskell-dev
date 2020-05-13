ghcup
-----

``ghcup`` installs the Glasgow Haskell Compiler from the official
release channels, enabling you to easily switch between different
versions. It maintains a self-contained ``~/.ghcup`` directory.

Install
-------

* Follow instructions at https://www.haskell.org/ghcup/

Help
----

::

  $ ghcup --help

Where is my stuff?
------------------

``$HOME/.ghcup/bin`` contains:

* ``ghcup`` to set current ghc version in `PATH`, to install/remove ghc versions
* ``ghc`` to compile Haskell programs
* ``cabal`` to install Haskell packages and build Haskell projects

Your shell ``PATH`` is automatically setup by the installation script to
find these, you can also use ``source $HOME/.ghcup/env`` to set it up
any time.
