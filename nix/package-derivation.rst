Nix Package Derivation
======================

If you are new to nix read the `Nix getting started guide
<user-guide.rst>`_ first.

Nix is an "immutable" package manager, it can be used to build,
install, use and remove packages from different sources into the system
hosting nix. Before building a package, it builds and installs all the
dependencies of the package, if necessary. Moreover, the dependencies of
one package cannot affect other packages, each package can have its own
, potentially different, version of the same dependency.

.. contents:: Table of Contents
   :depth: 1

See `Nix Reference <getting-started-nix-reference.rst>`_ for nix
expression language and builtin functions.

Building Packages
-----------------

Packages in nix are self contained directory trees in nix store. These
directory trees are derived from source recipes that define the sources,
dependencies, environment, tools and the build process. The resulting
output artifacts are stored in a directory tree in the nix store. We
will call this directory tree a "derived object" and this process of
building it "derivation".

The directory name of a derived object contains a hash of all the
inputs, environment, tools and anything that is used in the build
process. If we are trying to build an object we first generate the key
of the object from its inputs and see if it is already present in the
store, and use that if it is present, otherwise build it from source.

Let's now go through the basics of defining and building a derived
object using a nix expression. We generalize the term "package" to
"derived object" or "derivation". The term "derivation" can also used to
refer to the process or the recipe used to build the object.

Derivations Recipe
~~~~~~~~~~~~~~~~~~

A derived object is described by a nix expression, usually in a
file, say ``package.nix``. The primitive nix operation to create a
derivation recipe is ``derivationStrict``. ``derivation`` and
``nixpkgs.stdenv.mkDerivation`` are higher level wrappers around this
primitive.::

  nix-repl> derivationStrict { system = "x86_64-darwin"; name = "dummy"; builder = "/usr/bin/env"; }
  { drvPath = "/nix/store/xs4l5mv0rfzidxh4d5pigka2nsjpdy1r-dummy.drv"; out = "/nix/store/2869jzplqdaipayhij966s3c5lxv83l3-dummy"; }

The key information that a derivation requires is the name of the derived
object and how to build it.

Let's write a simple example using ``derivation`` and see how it works::

  $ cat > package.nix
  derivation {
      system = "x86_64-darwin";
      name = "dummy";
      builder = "/usr/bin/env";
      src = ./.;
  }

The expression in ``package.nix`` calls ``derivation`` with a ``set``
type argument (a set of key value pairs) describing the derivation. It
says our package is called ``dummy`` and its source is in ``./.``
i.e. the current directory and it is built using the ``/usr/bin/env``
program.

``/usr/bin/env`` does nothing, it just prints its environment
variables. We have chosen this just to illustrate the environment
that nix uses to call the builder, which is important to write a real
builder.

Evaluating the Recipe
~~~~~~~~~~~~~~~~~~~~~

``nix-instantiate`` and ``nix-store`` are low level commands to build a
derived object. ``nix-build`` is a high level command to build derived
objects. It is built on top of the former two.

A nix expression that evaluates to a derivation, list of derivations or
a set of derivations at the top level can be used by ``nix-instantiate``
to "realize" those derivations. In other words, the expression must use an
outermost operation that translates to ``derivationStrict`` ultimately.

Let's evaluate our trivial example::

    $ nix-instantiate --eval package.nix
    { system = "x86_64-darwin";
      name = "dummy";
      src = /Users/harendra/tmp;
      builder = "/usr/bin/env";

      type = "derivation";
      drvAttrs = { builder = "/usr/bin/env"; name = "dummy"; src = /Users/harendra/tmp; system = "x86_64-darwin"; };
      outputName = "out";
      drvPath = <CODE>;
      outPath = <CODE>;
      all = <CODE>;
      out = ... ;
    }

``--eval`` partially evaluated the derivation to a set. The set contains the
attributes that we specified and some more. Note that the path ``./.`` got
translated to an absolute path. Now let's evaluate it more using the
``--strict`` option::

    $ nix-instantiate --eval --strict package.nix
    { ...
      drvPath = "/nix/store/qg1q1hk3rb7ci8fq3ldkhgqvqfnmnal8-dummy.drv";
      outPath = "/nix/store/n8k5fdcgv52qqk64sz2nv8azqrfili8z-dummy";
      all = [ out ];
      out = <SELF>;
      ...
    }

It got fully evaluated now, the store paths of the derivation
``drvPath`` and ``outPath`` are generated using a unique hash based on the
build environment. ``out`` refers to the result of this derivation and
``all`` is a list of all outputs of the derivation which is just ``out``
in this case.

Building the Derivation Spec
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let's actually generate the derivation spec now::

  $ nix-instantiate package.nix
  /nix/store/qg1q1hk3rb7ci8fq3ldkhgqvqfnmnal8-dummy.drv

Let's open ``/nix/store/qg1q1hk3rb7ci8fq3ldkhgqvqfnmnal8-dummy.drv`` and see::

  Derive
    ( [("out","/nix/store/n8k5fdcgv52qqk64sz2nv8azqrfili8z-dummy","","")]
    , []
    , ["/nix/store/9q6a8fnsqpvgp4czvby4q9pncmc88v67-tmp"]
    , "x86_64-darwin"
    , "/usr/bin/env"
    , []
    , [ ("builder","/usr/bin/env")
      , ("name","dummy")
      , ("out","/nix/store/n8k5fdcgv52qqk64sz2nv8azqrfili8z-dummy")
      , ("src","/nix/store/9q6a8fnsqpvgp4czvby4q9pncmc88v67-tmp")
      , ("system","x86_64-darwin")
      ]
    )

Everything that the final derived object depends on has to be in the nix store,
therefore, our source directory ``./.`` has been copied to
``/nix/store/9q6a8fnsqpvgp4czvby4q9pncmc88v67-tmp`` in the store, this
path is also passed to the builder as ``src`` environment variable.

The list at the end contains the environment variables that will be passed as
environment of the builder when it is invoked. We can use the following command
to print the environment::

    $ nix-store --print-env /nix/store/qg1q1hk3rb7ci8fq3ldkhgqvqfnmnal8-dummy.drv

Local Path Translation
~~~~~~~~~~~~~~~~~~~~~~

An important thing to note is that we have an attribute ``src =
./.`` referring to the current directory path. Any path type attribute
referring to a local path causes the file or the directory tree to
be copied to the store and its location in the store is put in the
environment variable::

  src=/nix/store/9q6a8fnsqpvgp4czvby4q9pncmc88v67-tmp

Also, note that the permissions of the tree are made read-only and the
timestamps are set to 01-Jan-1970.

We can access any artifacts in our current directory by using the above
translated path.

Building the Derivation
~~~~~~~~~~~~~~~~~~~~~~~

``nix-instantiate`` only created the derivation spec object and copied
the source to nix store. The output object does not exist yet. Let's
try creating it from the derivation spec.  Our builder does nothing but
prints its environment::

  $ nix-store --realise /nix/store/qg1q1hk3rb7ci8fq3ldkhgqvqfnmnal8-dummy.drv

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

In addition to the environment variables above, nix also passes the
attributes used in ``derivation``'s argument set - as environment
variables with the same names::

  ...
  name=dummy
  system=x86_64-darwin
  builder=/usr/bin/env
  ...

Lastly, it passes a default ``out`` environment variable pointing to a
directory where the builder is supposed to store its output artificats::

  ...
  out=/nix/store/n8k5fdcgv52qqk64sz2nv8azqrfili8z-dummy
  ...

Notice that nix cleans the environment before invoking the builder
process and sets only those variables that are strictly required and
even sets some of the variables (``HOME`` and ``PATH``) to "junk" values
so that defaults are not filled by the shell. This is to ensure an
isolated build environment. We used ``/usr/bin/env`` in this example for
illustration, but we are not supposed to use any path outside the nix
sandbox for building, we must have explicit dependencies on other nix
packages and use the paths of those.

Building with Nix Build
~~~~~~~~~~~~~~~~~~~~~~~

Instead of using the low level commands, we can just use ``nix-build`` to
perform the above steps in one go::

    $ nix-build package.nix

The output directory ``$out`` is symlinked as ``result`` in the current
directory.

Note: ``nix-build`` without any arguments works on ``default.nix`` in the
current directory.

An Example Package
------------------

XXX: we can possibly remove the function argument syntax from this??

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
``builtins.`` prefix e.g. we can use ``builtins.import`` or just ``import``.

``<nixpkgs>`` is a syntax that is used to refer to the first nix module
(better known as nix expression) named ``nixpkgs`` found in
``NIX_PATH``.  By default it would be the nix expression in
``$HOME/.nix-defexpr/channels/nixpkgs``. The evaluation of this
expressions returns a set named ``nixpkgs``. ``nixpkgs.*`` in the code
is just accessing members of this set.

The builtin function ``import`` brings in the result of a nix expression
in the current scope. For example, to bring in the ``nixpkgs`` set and
refer to it by the name ``nixpkgs`` we can use::

  let nixpkgs = import <nixpkgs> {};
  in nixpkgs.dockerTools.buildImage { ... }

``with import <nixpkgs> {};`` brings all the members of the set imported
by ``import <nixpkgs> {}`` into the current scope. For example, the package
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

Then we can call the function defined in ``packcheck.nix`` supplying the
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

Installing the package
~~~~~~~~~~~~~~~~~~~~~~

::

    $ nix-env -i ./result

Building user environments
--------------------------

We now know how to build a derived object from a recipe using
``nix-build``.  The derived object output from ``nix-build`` is stored
in the nix store and a ``result`` link to the object is made available
in the current directory or as specified on the command line.

We can go further and also create a user environment for the object and
link its artifacts from a user profile, making the artifacts available
for general use.

A user environment is a collection of derived objects linked into a standard
file system hierarchy under one root. ``.nix-profile`` is a user environment.

::

  $ cat myprofile.nix
  let nixpkgs = import <nixpkgs> {};
  in nixpkgs.buildEnv {
        name = "my-packages";
        paths = [ nixpkgs.pkgs.bc nixpkgs.pkgs.coreutils ];
        pathsToLink = [ "/share" "/bin" ];
        extraOutputsToInstall = [ "man" "doc" ];
     }

It would create a derived object ``my-packages`` containing ``/share``,
``/bin`` directories of the ``bc`` and ``coreutils`` packages.

The ``nix-env`` command creates new user environments whenever we install or
uninstall packages.

Build functions and derivations
-------------------------------

See `Nix Reference <getting-started-nix-reference.rst>`_ for nix
expression language and builtin functions.

The set ``nixpkgs`` consists of a lot of nix functions/builders in
addition to package derivations. These functions can be used in various
custom derivations.  See the reference guide mentioned above for
some common ones. For an authoritative source of all functions see
``$HOME/.nix-defexpr/channels/nixpkgs``.

Building Nix shell
------------------

``nix-shell file.nix`` starts a shell from the nix expression in
``file.nix`` ::

  with (import <nixpkgs> {});
  mkShell {
    buildInputs = [
      coreutils
      gmp
    ];

    shellHook = ''
      alias ll = "ls -l"
      export C_INCLUDE_PATH = "${gmp}/include"
    '';
  }

By default nix-shell spawns a shell from ``shell.nix`` if the filename argument
is not specified.

The file must specify a derivation. ``mkShell`` above generates a derivation.

Customizing Nix distribution
----------------------------

`Nix getting started guide <user-guide.rst>`_ describes how the
nix distribution works. The whole distribution or collection of packages
visible to nix commands are defined by the nix expression obtained by
evaluating ``$HOME/.nix-defexpr``. Packages derived from this source are
fetched, built and stored in the nix store. When packages are available in the
binary cache they are downloaded from the cache.

Picking a Nix distribution
~~~~~~~~~~~~~~~~~~~~~~~~~~

Within a nix expression, instead of picking nixpkgs from NIX_PATH or
configured nix channels, we can pick a specific version of nixpkgs::

  nixpkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/4da09d369baa2200edb9df27fe9c88453b0ea6cf.tar.gz") {}

This can be used to pin the code to a specific version. For stability use a
stable nixos release version or for most current release use nixos-unstable.

Customizing the Nix distribution
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

What nix packages are available to you is determined by the
``NIX_PATH``. The directories in the ``NIX_PATH`` are combined together
in a single nix expression, this nix expression is used by the nix
utilities to show available packages or to install packages.

We can customize the distribution we are using by:

* Specifying a config when importing nixpkgs ``import <nixpkgs> config``
* Using a global configuration file in ``~/.config/nixpkgs/config.nix``
* Specifying overlays using the ``~/.config/nixpkgs/overlays.nix`` file
* Specifying overlays using individual overlay files in the
   ``~/.config/nixpkgs/overlays directory.``
* Using environment variables

Config specification
~~~~~~~~~~~~~~~~~~~~

Configuration to customize nixpkgs is specified as a set with attributes ::

  {
    allowUnfree =
    allowUnfreePredicate =
    allowBroken =
    allowUnsupportedSystem =
    whitelistedLicenses =
    blacklistedLicenses = 
    allowInsecurePredicate = 
    permittedInsecurePackages =
    packageOverrides =
    overlays =
  }

Usually we skip the config when importing nixpkgs and default values of these
attributes are used::

  import <nixpkgs> {};

However we can use a config::

  import <nixpkgs> { allowUnfree = true; };

Configuration file
~~~~~~~~~~~~~~~~~~

XXX todo: move the distracting parts out in a let caluse. Explain those in
separate sections before the config example.

We can modify the source nix expression defining the nix distribution by using
the nix configuration file ``~/.config/nixpkgs/config.nix``. That way we
can change or override the packages visible to the system, and add our
own packages to it::

  {
    allowUnfree = true;
    allowUnfreePredicate =
        pkg: builtins.elem (lib.getName pkg) [ "flashplayer" "vscode" ];
    allowBroken = true;
    allowUnsupportedSystem = true;
    whitelistedLicenses = with stdenv.lib.licenses; [ amd wtfpl ];
    blacklistedLicenses = with stdenv.lib.licenses; [ agpl3 gpl3 ];
    allowInsecurePredicate = pkg: builtins.stringLength (lib.getName pkg) <= 5;
    # Checked only if allowInsecurePredicate is not defined
    permittedInsecurePackages =
        [
            "hello-1.2.3"
        ];
    # takes all available pkgs as an argument and returns a modified set
    # of packages.
    packageOverrides = pkgs:
        with pkgs;
        {
            # Write a shell script in nix store to setup paths
            # This is an example, you may not need this as this may already be
            # setup by nix.sh.
            myProfile =
                writeText "my-profile"
                    ''
                    export PATH=$HOME/.nix-profile/bin:/nix/var/nix/profiles/default/bin:$PATH
                    export MANPATH=$HOME/.nix-profile/share/man:/nix/var/nix/profiles/default/share/man:$MANPATH
                    export INFOPATH=$HOME/.nix-profile/share/info:/nix/var/nix/profiles/default/share/info:$INFOPATH
                    '';
            # define a custom package bundle
            myBundle = pkgs.buildEnv {
                name = "my-packages";
                paths = [
                  bc
                  coreutils
                  gdb
                  texinfoInteractive # for install-info command

                  # copy our shell script to user profile i.e. $out
                  (runCommand "profile" {}
                      ''
                      mkdir -p $out/etc/profile.d
                      cp ${myProfile} $out/etc/profile.d/my-profile.sh
                      ''
                  )
                ];
            pathsToLink = [ "/share" "/bin" ];
            extraOutputsToInstall = [ "man" "doc" ];

            # Copy info files to the info root node i.e. $out/share/info/dir
            postBuild =
                ''
                if [ -x $out/bin/install-info -a -w $out/share/info ]
                then
                  shopt -s nullglob
                  for i in $out/share/info/*.info $out/share/info/*.info.gz
                  do
                      $out/bin/install-info $i $out/share/info/dir
                  done
                fi
                '';
            };
        };
  }

See ``~/.nix-defexpr/channels/nixpkgslib/licenses.nix`` for a complete
list of licenses.

Environment variables
~~~~~~~~~~~~~~~~~~~~~

::

  $ export NIXPKGS_ALLOW_BROKEN=1
  $ export NIXPKGS_ALLOW_UNSUPPORTED_SYSTEM=1
  $ export NIXPKGS_ALLOW_UNFREE=1
  $ export NIXPKGS_ALLOW_INSECURE=1

Overrides
~~~~~~~~~

A package set is a dependency tree. Packages at the top of the tree
depend on packages below. If we override a package in this tree the
whole tree should be rebuilt to use the changed definition wherever the
package is used.

Note that overriding a package lower below may cause rebuilding of all
the packages that depend on it. To avoid rebuilding the whole world we
can push the override as far above in the tree as possible. For example,
if one of the packages that depends on "git" requires a changed definition
of git then we can override that package to use a new "git" instead of
overriding the original "git".

The functions below are basic low level constructs to override
individual packages in the package set.

Override is used on a function to override its arguments.  Wherever a
function is called to build the whole package set, it is effectively
replaced by its overridden definition. ``makeOverridable`` can be used to make
a function overridable, providing a ``override`` attribute that can be called
to override its arguments.

::

  <pkg>.override          # override the arguments passed to an overridable function "pkg".
  <pkg>.overrideAttrs     # override the attribute set passed to a stdenv.mkDerivation call
  <pkg>.overrideDerivation # override a derivation using an old derivation
  lib.makeOverridable


* https://nixos.org/manual/nixpkgs/stable/#chap-overrides
* https://nixos.org/guides/nix-pills/override-design-pattern.html
* https://nixos.org/guides/nix-pills/nixpkgs-overriding-packages.html

Overlays
~~~~~~~~

Override is used to override function definitions whereas overlays
override sets. We can combine a set definition with a new overridden
definition to create a new resulting set. This can be used to override
the entire set of packages (``nixpkgs``).

Overlays are Nix functions which accept two arguments, conventionally
called ``self`` and ``super``, and return a set of packages. The first
argument (self) corresponds to the final package set. The second
argument (super) corresponds to the result of the evaluation of the
previous stages of Nixpkgs. It does not contain any of the packages
added by the current or following overlays::

  self: super:
      {
        boost = super.boost.override {
          python = self.python3;
        };
        rr = super.callPackage ./pkgs/rr {
          stdenv = self.stdenv_32bit;
        };
      }

The value returned by this function should be a set similar to
``pkgs/top-level/all-packages.nix``, containing overridden and/or new
packages.

* See https://nixos.wiki/wiki/Overlays for a good explanation

Applying Overlays
.................

1) When importing nixpkgs::

    import <nixpkgs> { overlays = [ overlay1 overlay2 ]; }.

2) Using ``~/.config/nixpkgs/overlays.nix`` file
3) By creating individual overlay files in the
   ``~/.config/nixpkgs/overlays`` directory.
4) By calling the following::

    pkgs.extend
    pkgs.appendOverlays

This is more expensive as it recomputes the nixpkgs fixed point.

packageOverrides
~~~~~~~~~~~~~~~~

``packageOverrides`` acts as an overlay with only the ``super``
argument. It is therefore appropriate for basic use, but overlays are
more powerful and easier to distribute.

We can modify the attibutes of a package derivation or add new package
derivations to the set of packages in ``nixpkgs`` ::

  {
    packageOverrides = pkgs: rec {
      coreutils = pkgs.coreutils.override { ... };
    };
  }

Installing Additional Package Components
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For example, if we want to install the dev version of the gmp package to get
the gmp.h header file installed in ~/.nix-profile/include ::

  {
    packageOverrides = super:
    {
        gmp =
            super.gmp.overrideAttrs (oldAttrs:
                {
                  meta = oldAttrs.meta // { outputsToInstall = oldAttrs.meta.outputsToInstall or [ "out" ] ++ [ "dev" ]; };
                }
            );
    };
  }

Working with Nix Store
----------------------

Nix Global Data
~~~~~~~~~~~~~~~

The whole nix distribution consists of ``/nix/var`` and ``/nix/store``.

The ``/nix/var`` directory contains top level control information about the
whole nix installation. ``/nix/var/nix`` contains:

* ``profiles`` - default user profiles, the top level point from where a user
  accesses the distribution.
* ``gcroots`` - derivations reachable from this are not removed
* ``userpool``
* a sqlite database (what does it have?)

Nix Store
~~~~~~~~~

Nix store consists of directories that may contain a self-contained
package or a derivation (.drv suffix). Each such package may depend on
other packages installed in the store. The whole tree is rooted at user
profiles. Each path in the store is a tree consisting of a package and
its dependencies.

The ``nix-store`` command can be used to manipulate the contents of the
nix store. See ``nix-store --help``.

Subtree/path level:

Create:

* ``nix-store --add`` - add a path to nix-store
* ``nix-store --realise`` - make sure the given store path tree is complete and
  valid, if not fetch it or build it.
* ``nix-store --restore`` - restore a path tree from a nix archive (tar)
* ``nix-store --import`` - import an exported archive (see --export)
* ``nix-store --load-db`` - load a nix db for the path tree (see --dump-db)

Read:

* ``nix-store --query`` - query info about a path
* ``nix-store --print-env`` - environment of a .drv path
* ``nix-store --read-log`` - print build log of a path
* ``nix-store --verify-path`` - verify a path
* ``nix-store --dump`` - dump a path tree as nix archive (tar)
* ``nix-store --export`` - export an archive for non nix-store purposes
* ``nix-store --dumpdb`` - dump nix db for the path tree

Update:

* ``nix-store --repair-path`` - repair a path

Delete:

* ``nix-store --delete`` - delete if nobody is using it

Store level:

* ``nix-store --serve`` -  provide access to the whole store over stdin/stdout
* ``nix-store --gc`` - garbage collect
* ``nix-store --verify`` - verify the consistency of the nix database

Further Reading
---------------

You are now equipped with all the basic knowledge of Nix and
Nix packaging, you can now move on to the `Nix Haskell Development Guide
<haskell-development.rst>`_.

References
----------

* https://nix.dev/tutorials/towards-reproducibility-pinning-nixpkgs.html#pinning-nixpkgs
* https://ghedam.at/15978/an-introduction-to-nix-shell
