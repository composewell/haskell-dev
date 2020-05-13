Install `ghc` and `cabal` on Linux using Shell
==============================================

The following example is for Debian 10 For other
distributions replace the tar file (``.tar.xz``) URL below
with an appropriate ghc tar image URL `from this page
<https://www.haskell.org/ghc/download_ghc_8_10_1.html>`_.

Install ``ghc``
---------------

Install ``ghc`` version ``8.10.1`` under ``$HOME/.local/ghc/``::

    $ mkdir -p $HOME/.local/ghc/installer
    $ cd $HOME/.local/ghc/installer
    $ wget https://downloads.haskell.org/~ghc/8.10.1/ghc-8.10.1-x86_64-deb10-linux.tar.xz | tar -Jxf
    $ cd ghc-8.10.1
    $ ./configure --prefix=$HOME/.local/ghc/8.10.1 
    $ make install
    $ cd
    $ rm -rf $HOME/.local/ghc/installer

Setup your shell to find the newly installed ``ghc``.  For example, if
you are using ``bash`` shell::

    $ echo 'export PATH=$HOME/.local/ghc/8.10.1/bin' >> $HOME/.bash_profile
    $ source $HOME/.bash_profile

Install ``cabal``
-----------------

Install ``cabal`` version 3.2 under ``$HOME/.local/bin/``::

    $ mkdir -p $HOME/.local/bin
    $ wget -O - https://downloads.haskell.org/~cabal/cabal-install-3.2.0.0/cabal-install-3.2.0.0-x86_64-unknown-linux.tar.xz | tar -JOfx > $HOME/.local/bin

Setup your shell to include ``$HOME/.local/bin/`` in your `PATH`. For example,
if you are using ``bash``::

    $ echo 'export PATH=$HOME/.local/bin' >> $HOME/.bash_profile
    $ source $HOME/.bash_profile
