Getting Started with Nix
========================

.. contents:: Table of Contents
   :depth: 1

What is Nix?
------------

Nix is package manager that can be installed on Linux or Mac OS. Unlike
other package managers Nix does not use one single global view of some
fixed versions of packages. It can create a custom environment with
whatever versions of packages you need and their dependencies, on the
fly.

The whole package tree can be defined using a specification with Nix
expressions. Any such specification can be installed from scratch and
multiple such environments can be installed at the same time. One can switch
quickly from one environment to another, and use custom environments for
different tasks.

Requirements
------------

Nix may take a fair amount of disk space (e.g. 5 GB to begin with)
as it installs a self contained package ecosystem with all the
dependencies. It does not depend on most system libraries. The initial
install may take some time as it may have to install a fair number
of packages. The used space grows as you use more packages, however,
you can use ``nix-collect-garbage`` to recover space from unused
packages/derivations.

For powerful use, you may have to spend some time to learn the Nix
expression language to use Nix effectively, but its declarative and easy
to learn, and a small subset is enough to get going.

Advantages
----------

Of course, you get a very powerful way to manage packages.  On Mac
OS you can get rid of package managers like Homebrew, MacPorts for
installing third party applications and just use Nix instead. Haskell
developers can get rid of the space used by cabal or stack for package
store and use nix installed packages instead.  The disk space that nix
takes can be compensated by these savings.

Terminology
-----------

+------------------------+----------------------------------------------------+
| Nix                    | Package manager available on Unix (Linux/MacOS)    |
+------------------------+----------------------------------------------------+
| NixOS                  | Linux distribution that uses Nix as pkg mgr        |
+------------------------+----------------------------------------------------+
| Nixpkgs                | Collection of packages available via Nix           |
+------------------------+----------------------------------------------------+

Install Nix
-----------

Nix installs its shared package store in ``/nix``.

Quick install:

* On Linux: ``curl https://nixos.org/nix/install | sh``
  On Debian Linux you may need: ``sudo sysctl kernel.unprivileged_userns_clone=1``
* On Mac OS::
  
    $ sh <(curl https://nixos.org/nix/install) --darwin-use-unencrypted-nix-store-volume

  On older Macs (https://support.apple.com/en-us/HT208862) you may see
  this error::

    error: refusing to create Nix store volume because the boot volume is
           FileVault encrypted, but encryption-at-rest is not available.
           Manually create a volume for the store and re-run this script.
           See https://nixos.org/nix/manual/#sect-macos-installation

  This means encryption is enabled on your mac but you do not have the
  hardware (T2 chip) to encrypt at rest so the nix volume may remain
  unencrypted. You can still install anyway by manually creating the
  ``/nix`` volume::

      $ sudo diskutil apfs addVolume disk1 APFS 'Nix Store' -mountpoint /nix
      $ diskutil list
          ...
          6:                APFS Volume Nix Store               20.5 KB    disk1s6

      # Add the following line to fstab using vifs (without the #)
      # LABEL=Nix\040Store /nix apfs rw,nobrowse
      $ sudo vifs

  And then run the above script again.

* `Nix download page <https://nixos.org/nix/download.html>`_

Post Install
------------

Nix provides a shell script to set up the shell ``PATH`` and a few other
environment variables to access the nix commands.  Do not forget to run
this script after the installation is finished to make the nix config
effective in the current shell or you have to login again::

  $ source ~/.nix-profile/etc/profile.d/nix.sh

This is automatically added to your shell profile, if not you can add
it manually to your shell profile.  To verify that your installation is
working correctly::

    $ nix-env --version
    nix-env (Nix) 2.3.6

Nix User Profiles
~~~~~~~~~~~~~~~~~

A nix profile is a self contained directory consisting of a particular
set of installed programs and libraries.  A user may create multiple nix
profiles.  ``$HOME/.nix-profile`` is a symlink to the active profile::

    $ ls -al ~/.nix-profile
    lrwxr-xr-x  1 harendra  staff  47 Jul  8 12:32 /Users/harendra/.nix-profile -> /nix/var/nix/profiles/per-user/harendra/profile

``~/.nix-profile/bin`` contains the binary executables available in the
current profile, ``~/.nix-profile/lib`` contains the libraries and so
on. ``~/.nix-profile/bin`` is placed in your OS shell ``PATH`` so these
programs are available in your shell.

Useful Commands
---------------

On fresh install, the default nix profile would have only the ``nix``
package installed which provides the nix package manager binaries. To
check out all the nix commands available::

  $ ls -al ~/.nix-profile/bin

It contains the following commands, among others:

* nix-channel
* nix-env
* nix-build
* nix-shell
* nix-store

All ``nix-*`` commands are symlinks to the `nix` master binary which behaves
in different ways depending on what name it is invoked with.

To know more details about any of the nix commands use `--help`::

  $ nix --help
  $ nix-channel --help

Nix Packages and Nix Expressions
--------------------------------

Nix package manager installs nix packages and their dependencies
and makes them available in a user environment.  A nix package is
described using a Nix expression. A Nix expression is a recipe
(known as a derivation) to build (derive) binaries from a source
package. However, it first tries to install prebuilt binaries from
the `nix binary repository <https://cache.nixos.org>`_. Nix expressions
for all packages can be found in the `nix expression repository
<https://github.com/NixOS/nixpkgs>`_.

`See this reference manual <https://nixos.org/nixpkgs/manual/>`_ for
defining nix packages using nix expressions.

Nix Channels
------------

Nix packages are distributed through nix channels. Channels are specified in
https://github.com/NixOS/nixpkgs-channels.  Branches in that repository
correspond to available channels. Examples of some available channels are:

+------------------------+----------------------------------------------------+
| nixpkgs-unstable       | Packages for Nix on Mac/Linux                      |
+------------------------+----------------------------------------------------+
| nixos-16.03            | Packages for NixOS 16.03                           |
+------------------------+----------------------------------------------------+
| nixos-unstable         | Up to date packages for NixOS                      |
+------------------------+----------------------------------------------------+

Use the ``nix-channel`` command to manage the channels ::

  $ nix-channel --list
  nixpkgs https://nixos.org/channels/nixpkgs-unstable
  $ nix-channel --update

Nix User Environments
---------------------

The directory ``/nix`` is a global store containing all packages, each
one installed in its own directory named using a hash of the full
configuration using which the package was built. Since then names are
hashed with full config, we may have multiple versions of a package in
the store. A specific user environment is composed using the packages
from the global store, consisting of a custom set of programs and
libraries.

A nix profile is a user environment contained in a directory in the
filesystem.  A user can have multiple such profiles with different
set of programs and libraries installed in them. We can derive
another profile using an existing profile as the base.  The file
``$HOME/.nix-profile`` points to one of the profile directories, it is
called the active porifle. When a user installs a program without
explicitly specifying a profile it is installed in the active profile
directory.

The directory ``/nix/var/nix/profiles`` contains the default nix created
profiles. ``~/.nix-profile`` is a symlink to one of those profiles::

  $ lal $HOME/.nix-profile
  lrwxr-xr-x  1 harendra  staff  47 Jun 12 19:37 $HOME/.nix-profile -> /nix/var/nix/profiles/per-user/harendra/profile

Let's take a look at the profiles of current user just after installing
nix::

  $ ls -al /nix/var/nix/profiles/per-user/$USER/profile*
  lrwxr-xr-x  1 harendra  staff   14 Jun 12 10:51 profile -> profile-2-link
  lrwxr-xr-x  1 harendra  staff   60 Jun 12 10:51 profile-1-link -> /nix/store/mfxdq39kisqzdhngm4wx505fxny7144f-user-environment
  lrwxr-xr-x  1 harendra  staff   60 Jun 12 10:51 profile-2-link -> /nix/store/p627zifc00wkfyja0fphajzybpbc0sf6-user-environment

We see that ``profile`` points to ``profile-2-link``.  When programs
are installed or uninstalled within a profile, the old version of the
profile is kept intact and a new version is created and activated,
these are called generations of a profile. ``profile-2-link`` is the
2nd generation of the profile. You can check the differences between
``profile-1-link`` and ``profile-2-link`` by diffing their targets::

    $ diff /nix/store/mfxdq39kisqzdhngm4wx505fxny7144f-user-environment /nix/store/p627zifc00wkfyja0fphajzybpbc0sf6-user-environment

The first generation had only ``nix`` installed in it and the second generation
has one more package (ca certificates) installed.

* https://nixos.org/nix/manual/#sec-profiles

Nix Package Manager
-------------------

``nix-env`` is the nix command for package management. ``nix-env`` searches
or installs packages from the default nix channel or the channels added using
``nix-channel``.

Query all installed packages::

  $ nix-env -q       # --query, installed packages in the active profile
  $ nix-env -qa      # --available, available packages

Query selected packages::

  $ nix-env -qa '.*cabal.*' # packages matching a regex
  cabal-install-3.2.0.0
  cabal2nix-2.15.3
  ...

  $ nix-env -qa '.*(cabal|ghc).*'

You can also `search nix packages here
<https://nixos.org/nixos/packages.html?channel=nixpkgs-unstable>`_.

Packages by Attributes
----------------------

Nix packages are grouped under an attribute hierarchy starting with
``nixpkgs`` at the top level.  To list a package attribute path use
``-P``::

  $ nix-env -qaP '.*cabal.*'
  nixpkgs.cabal-install        cabal-install-3.2.0.0
  nixpkgs.cabal2nix            cabal2nix-2.15.3
  ...

We can select packages by specifying an attribute path using the ``-A``
option.  Note that regex patterns do not work with attributes.  The
default attribute path is the top level attribute ``nixpkgs``::

  $ nix-env -qaP -A nixpkgs

However, not all packages are available directly under the top level
attribute. For all other attributes we need to specify the full
attribute path explicitly to search the packages::

  $ nix-env -qaP -A nixpkgs.haskell.compiler
  nixpkgs.haskell.compiler.ghc8101                 ghc-8.10.1
  nixpkgs.haskell.compiler.integer-simple.ghc8101  ghc-8.10.1
  ...

  $ nix-env -A nixpkgs.haskellPackages -qaP '.*streamly.*'
  nixpkgs.haskellPackages.streamly  streamly-0.7.2

Important Note: If you are inside a ``nix-shell --pure`` environment the
``nix-env`` command above may not show any packages.

Installing packages
-------------------

When installing a package nix would fetch/build all the dependencies
that are not already available in the store. On the first invocation a
lot of dependencies may be built/fetched.

We recommend that you always install by attributes, using the `-A` flag. If you
do not do that, often you may install the wrong package because when there are
multiple matches it installs the first package::

  $ nix-env -i -A nixpkgs.ghc # --install --attr

To install a Haskell package::

  $ nix-env -i -A nixpkgs.haskellPackages.streamly

Profile Generations
-------------------

Let's look at the nix profiles directory after installing ``ghc``::

  $ ls -al /nix/var/nix/profiles/per-user/$USER/profile*
  lrwxr-xr-x  1 harendra  staff   14 Jun 12 14:44 profile -> profile-3-link
  lrwxr-xr-x  1 harendra  staff   60 Jun 12 10:51 profile-1-link -> /nix/store/mfxdq39kisqzdhngm4wx505fxny7144f-user-environment
  lrwxr-xr-x  1 harendra  staff   60 Jun 12 10:51 profile-2-link -> /nix/store/p627zifc00wkfyja0fphajzybpbc0sf6-user-environment
  lrwxr-xr-x  1 harendra  staff   60 Jun 12 14:44 profile-3-link -> /nix/store/5phrf9z4xjsbd0lscli06bvxpdvzy926-user-environment

Note that the previous generation of the profile (``profile-2-link``) is
preserved as it is and a new generation is created (``profile-3-link``)
which has the newly installed package as well as the earlier packages.
The ``profile`` now points to the new generation. The binaries for the
newly installed package ``ghc`` are now available in your ``PATH`` from
``~/.nix-profile/bin``.

If we want to switch to the previous generation of the profile::

    $ nix-env --list-generations
    $ nix-env --switch-generation 2

This will restore us to the previous state when there was no ``ghc`` installed.
``ghc`` will no longer be available in your ``PATH``. Note you can always
switch back to earlier generation.

To switch to a previous generation, we can use a convenience command::

    $ nix-env --rollback

To permanently delete a generation::

    $ nix-env --delete-generation 2

To actually recover the space you have to run ``nix-collect-garbage``.

TBD: how to know what is different between two generations? diff the
directories?

Using a different Nix "repository" path
---------------------------------------

The nix package repository is defined by a nix expression.  The default
nix expression used by nix commands is ``$HOME/.nix-defexpr``. The
``-f`` command can be used to specify a different nix expression. The
default ``nix-env -qaP ghc`` command is equivalent to::

  $ nix-env -f '$HOME/.nix-defexpr' -qaP ghc

Query a package in ``nixpkgs`` attribute name space::

    $ nix-env -f '$HOME/.nix-defexpr/channels/nixpkgs' -qaP -A haskell.compiler

Alternatively, we can use ``<nixpkgs>`` syntax, which means search for
``nixpkgs`` in the ``NIX_PATH``::

    $ echo $NIX_PATH
    $HOME/.nix-defexpr/channels
    $ nix-env -f '<nixpkgs>' -qaP -A haskell.compiler

Query packages available under a Nix expression defined by a file or URL::

  $ nix-env -f ./foo.nix -qa
  $ nix-env -f https://github.com/NixOS/nixpkgs/archive/master.tar.gz -qa

Install a package from a Nix expression ::

  $ nix-env -f ~/foo.nix -i '.*' # install all derivations from foo.nix

Working with Profiles
---------------------

Which profile am I using currently? Check where the symlink
``~/.nix-profile`` is pointing::

  $ ls -al ~/.nix-profile
  lrwxr-xr-x  1 harendra  staff  47 Jul  8 12:32 /Users/harendra/.nix-profile -> /nix/var/nix/profiles/per-user/harendra/profile

A profile is nothing but a directory containing a user environment. When
we say ``profile`` we refer to a path of the profile directory.  There
is nothing special about a profile directory, it is a regular directory
which could be located anywhere in the filesystem. The default profile
created by nix is at::

  $ ls -al /nix/var/nix/profiles/per-user/$USER/profile*

A new profile gets created when you try to install something using
a profile path that does not exist. You can
switch to a profile path that points to a non-existing directory.  It is
considered as a blank profile without anything installed in it. Usually,
you would want to install at least ``nix`` in the new profile so that
``nix`` commands keep working when you switch to the new profile::

  $ nix-env --profile ./custom_profile -iA nixpkgs.nix

It will create a directory named ``custom_profile`` in the current
directory and install ``nix`` in it. Also install ``nixpkgs.cacert``
so that you are able to install packages by downloading from the nixos
cache::

  $ nix-env --profile ./custom_profile -iA nixpkgs.cacert

Since profiles are just self contained directories you can move them anywhere
in the file system and still refer to them by their new path when needed.

Switch to the new profile::

  $ nix-env --switch-profile ./custom_profile
  # Alternatively
  $ nix-env -S ./custom_profile

Pitfall: Do not use a directory with existing non-nix installed contents as a
profile directory.

Pitfall: ``nix`` does not verify whether the path you are switching to
is a valid nix profile or not. You can use any path even a non-existing one.

If you make a spelling mistake in the path of the profile when switching
to a profile it may end up pointing to a yet non-existing profile or to
a directory which is not a nix profile.  Because of that you may not be
able run any nix commands any more. If that happens you can restore to
the default profile path show above or see the diagnostics section to
know other ways to recover from that situation.

If by mistake you gave a wrong existing directory as profile path
``nix`` will happily install the programs in that directory.

Installing a program from another profile::

  $ nix-env -i --from-profile /nix/var/nix/profiles/foo ghc

TBD: How does nix track the profiles, won't the programs installed in these
profiles get garbage collected on ``nix-collect-garbage``? Should we create the
profiles within the nix store profiles directory so that it can track?

TBD: deleting profiles

Nix Shell
---------

In a standard OS Shell, nix programs and libraries available to you are the
ones that are installed in the current nix profile. You can have multiple
profiles and switch profiles to switch environments.

``nix-shell`` creates an ad-hoc user environment directory in nix
store, installs requested packages in it and starts an OS shell with
``PATH`` and other environment variables setup to access the binaries
and the libraries in the user environment.

For example to start a shell with ``gcc`` and ``coreutils`` packages
installed::

  $ nix-shell --packages "[gcc coreutils]"
  these paths will be fetched (41.00 MiB download, 175.04 MiB unpacked):
    /nix/store/3hl9jc7dgk2qg38xfwg14w10kbdnpj78-mpfr-4.0.2
    ...

  [nix-shell:~]$

``--packages`` option takes a list (``[gcc coreutils]``) of packages
as argument.  Note that we do not use ``nixpkgs.`` prefix here when
specifying the packages, that is implicit.

The shell creates an ad-hoc immutable user environment in the nix store and
installs the packages in it::

  [nix-shell:~]$ which gcc
  /nix/store/3sbzwr62w0nwglsdaiskj7r7sfbwxy69-gcc-wrapper-9.3.0/bin/gcc

  [nix-shell:~]$ echo $PATH

If you want more packages to be added later you need to exit the shell
and start a new shell with the new set of packages. It will create a new
sandbox. However, the creation of the sandbox may mostly involves setting up
some symlinks if the packages being installed are in the nix store already.

See ``nix-shell --help`` for more details.

Caching of packages
~~~~~~~~~~~~~~~~~~~

The sandbox created by the shell is cached and is reused every time the
same configuration is requested. Therefore, next time the shell starts
quickly.  Note that the sha256 hash used in the location of the sandbox
is hashed using the config of the sandbox.

Upgrade
-------

Upgrade::

  $ nix-env --upgrade ghc

Uninstalling packages
---------------------

::

  $ nix-env --uninstall firefox
  $ nix-collect-garbage

Garbage Collection
------------------

The nix store can grow over time accumulating packages which are
no longer required thus unnecessarily consuming disk space. Use
``nix-collect-garbage`` to free up space.

``nix-collect-garbage`` deletes all the objects in the nix store which are not
being pointed to by anyone else in the nix store. Starting from the default
profiles?

TBD: pinning derivations.

Uninstall Nix
-------------

Nix stores its files only at two places ::

    $ rm -rf ~/.nix-*
    $ sudo rm -rf /nix

Nix package Attributes
----------------------

Top level::

  nixpkgs

General packages::

  nixpkgs.pkgs

Darwin (Mac OS)
~~~~~~~~~~~~~~~

Apple sdk and frameworks::

  nixpkgs.pkgs.darwin
  nixpkgs.pkgs.darwin.apple_sdk
  nixpkgs.pkgs.darwin.apple_sdk.frameworks

llvm
~~~~

gcc compatible development environment with C compiler and more::

    nixpkgs.llvm
    nixpkgs.llvmPackages

Diagnostics
-----------

Q: How do I get more information to diagnose the issue?

A: Most commands support a verbose option ``-v`` which can provide a lot more
information for better debugging.

Q: How to print the store path of a package?

A::

  $ nix-build --no-out-link "<nixpkgs>" -A zlib
  /nix/store/mi9z1dmjp95n90lfy3rqifqzxphvnyzh-zlib-1.2.11

Q: ``nix-env -qa`` is not showing any packages.

A: Make sure you are not in a ``nix-shell --pure`` environment. Check if your
``NIX_PATH`` variable is set correctly.

Q: After switching to a new profile all nix commands stopped working.

A: ``nix-env -S`` is a pretty dumb command, it just replaces
  ``~/.nix-profile`` symlink with whatever you give it. If that path
  points to an existing profile you have successfully switched to the
  new profile. However, if that profile does not exist you have switched
  to a "new" non-existing profile which is completely empty, not even
  nix commands exist in that profile. You now need to install something
  in that profile, but you no longer have access to even ``nix-env`` to
  actually do that. Nix maintainers refuse to fix or even document this
  stupid problem, see https://github.com/NixOS/nix/issues/1396.

  To recover the situation you can either find the path to ``nix-env`` (e.g.
  using ``find /nix -type l -name nix-env``) and use
  it to initialize the new profile with something useful (e.g. ``nix-env -i
  nix``) or you can manually link ``~/.nix-profile`` to a valid profile
  from ``/nix/var/nix/profiles/per-user/$USER/``. For example::

  $ ln -s -f /nix/var/nix/profiles/per-user/$USER/profile-2-link/ ~/.nix-profile

Q: Getting ``SSL peer certificate or SSH remote key was not OK (60)`` error
   when installing a package.

A: The easiest solution to this problem is to switch to your default
  profile which has the ``nixkpkgs.cacert`` package installed, and then
  run the install command for the profile in which you want to install a
  package::

    $ nix-env -S /nix/var/nix/profiles/per-user/$USER/profile
    $ nix-env --profile <profile-path> -iA <package>

  If you want to be able to install packages from a profile without switching to
  the default profile make sure it has ``nixpkgs.cacert`` installed::

    $ nix-env --profile <profile-path> -iA nixpkgs.cacert

  If it is installed, check if the environment variable
  ``NIX_SSL_CERT_FILE`` is correctly set::

    $ echo $NIX_SSL_CERT_FILE
    $HOME/.nix-profile/etc/ssl/certs/ca-bundle.crt

  If not then you may want to source your shell profile::

    $ source ~/.bash_profile

  If that is not correctly setup then you can directly source the nix setup
  script::

    $ source $HOME/.nix-profile/etc/profile.d/nix.sh

Q: How to deal with "packages x and y have the same priority..."? ::

  $ nix-env -iA nixpkgs.haskellPackages.streamly
  installing 'streamly-0.7.2'
  building '/nix/store/7c6lvazaxyr6ndxypzv56bss6y8rgl5k-user-environment.drv'...
  error: packages '/nix/store/bbrbfv3lzlph2zx7yyd719wi5v3x25zx-streamly-0.7.2/lib/links/libgmpxx.4.dylib' and '/nix/store/4pxkzpv66pm0lwrcjns1v88wn3byi75b-hoogle-5.0.17.15/lib/links/libgmpxx.4.dylib' have the same priority 5; use 'nix-env --set-flag priority NUMBER INSTALLED_PKGNAME' to change the priority of one of the conflicting packages (0 being the highest priority)
  builder for '/nix/store/7c6lvazaxyr6ndxypzv56bss6y8rgl5k-user-environment.drv' failed with exit code 1
  error: build of '/nix/store/7c6lvazaxyr6ndxypzv56bss6y8rgl5k-user-environment.drv' failed

A: You may run into such errors if you are installing many different
   programs/libraries in the same nix profile and two or more of those
   depend on a different version of the same package, in this case
   ``libgmpxx``. If possible, use different profiles for different tasks
   and their environments. If you really have to install both the
   programs in the same profile then you can change the priority of the
   already installed package (in this case ``hoogle-5.0.17.15``) as
   suggested in the error message::

     $ nix-env --set-flag priority 4 hoogle-5.0.17.15
     setting flag on 'hoogle-5.0.17.15'

     $ nix-env -iA nixpkgs.haskellPackages.streamly
     installing 'streamly-0.7.2'
     building '/nix/store/5jcny4113np1il4yf56kra6iyg6h9aj2-user-environment.drv'...
     created 342 symlinks in user environment

Q: On macOS, getting this error::

    cbits/c_fsevents.m:1:10: error:
         fatal error: 'CoreServices/CoreServices.h' file not found
      |
    1 | #include <CoreServices/CoreServices.h>
      |          ^
    #include <CoreServices/CoreServices.h>
             ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    1 error generated.
    `cc' failed in phase `C Compiler'. (Exit code: 1)

A: The ``CoreServices`` framework is missing::

  $ nix-env -iA nixpkgs.pkgs.darwin.apple_sdk.frameworks.CoreServices
  installing 'apple-framework-CoreServices'
  building '/nix/store/04yl425g4lp3ld5xlzv04b7n8zbmzi3y-user-environment.drv'...
  created 71 symlinks in user environment

Quick References
----------------

Install
~~~~~~~

* `Nix package manager install script <https://nixos.org/nix/install>`_
* `Nix package manager download <https://nixos.org/nix/download.html>`_

Learning
~~~~~~~~

* https://nixos.org/nixos/nix-pills/ The best place to start

Reference Docs
~~~~~~~~~~~~~~

* `Nix Package Manager (Nix) Manual <https://nixos.org/nix/manual/>`_
* `Nix Package Collection (nixpkgs) and language reference <https://nixos.org/nixpkgs/manual/>`_

  * `Writing Nix expressions <https://nixos.org/nix/manual/#chap-writing-nix-expressions>`_
  * `Nix profiles <https://nixos.org/nix/manual/#chap-writing-nix-expressions>`_

* `Nix package manager home page <https://nixos.org/nix/>`_
* `Nix Cookbook <https://nix.dev/>`_

Search Packages
~~~~~~~~~~~~~~~

* `Search nixos/nix packages <https://nixos.org/nixos/packages.html>`_

Repositories
~~~~~~~~~~~~

* `Nix package expression repository <https://github.com/NixOS/nixpkgs>`_
* `Nix package distribution channels <https://github.com/NixOS/nixpkgs-channels>`_
* `NixOS prebuilt binary cache <https://cache.nixos.org>`_

Others
~~~~~~

* `NixOS <https://nixos.org/>`_
* `Nix based CI <https://github.com/mightybyte/zeus>`_
