Nix Expression Language
-----------------------

You can use ``nix repl`` to try out the language interactively. Any expression
that is typed in the repl is evaluated and its result is printed.

Operators: See https://nixos.org/manual/nix/stable/#table-operators

Comments::

    # this is a comment
    /* this is a comment */

Null : ``null``

Number Literals::

    1
    1.1
    0.1e1
    (- 1)

Arithmetic operations::

    1 + 2
    1 - 2
    1 * 2
    1 / 2 (note 1/2 would be a path string)
    1 < 2
    1 <= 2
    1 > 2
    1 >= 2

Strings
~~~~~~~

Strings::

    "hello world"
    'hello world'

Indented Strings::

  ''
    This is the first line.
    This is the second line.
      This is the third line.
  ''

Special strings: URI Strings can be written without quotes::

    http://hello.world

Special strings: Paths (at least one "/" is needed to deem it as a path type)::

    ./.       # current directory
    /.        # root directory
    a/b
    ~/a       # expand ~ to home directory
    <nixpkgs> # search nixpkgs in NIX_PATH

String concatenation::

    "hello" + "world"

String interpolation, expand an expression in a string::

    let expr = "world" in "hello ${expr}"

Since ${ and '' have special meaning in indented strings, you need a way
to quote them. $ can be escaped by prefixing it with '' (that is, two
single quotes), i.e., ''$. '' can be escaped by prefixing it with ',
i.e., '''. $ removes any special meaning from the following $. Linefeed,
carriage-return and tab characters can be written as ''\n, ''\r, ''\t,
and ''\ escapes any other character.

Local variables
~~~~~~~~~~~~~~~

  let
    x = "foo";
    y = "bar";
  in x + y

Lists
~~~~~

whitespace separated items in square brackets::

    [ 123 ./foo.nix "abc" (f { x = y; }) ]

Concatenating lists::

    [ 1 2 ] ++ [ 3 4 ]

Booleans
~~~~~~~~

Booleans: ``true``, ``false``

Conditionals::

    if e1 then e2 else e3

Logical Operations::

    ! x
    x == y
    x != y
    x && y
    x || y
    x -> y (!x || y)

Sets
~~~~

Sets: name value pairs (attributes), terminated by ``;`` and enclosed in
curly brackets::

  { x = 123;
    text = "Hello";
    y = f { bla = 456; };
  }

Recursive sets (defined with a ``rec`` keyword), attributes can refer to
each other::

  rec {
    x = y;
    y = 123;
  }.x

SELECT operator: Attributes can be selected from a set using the ``.``
operator.  Default value in an attribute selection can be provided using
the ``or`` keyword. For example::

  { a = "Foo"; b = "Bar"; }.c or "Xyzzy"

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
``inherit x y z`` can be used to inherit multiple attrs at the same time.
``inherit (pkgs) zlib`` implies ``zlib = pkgs.zlib``
``inherit (pkgs) zlib coreutils`` can be used to inherit multiple attrs from
pkgs.

with:

``with set; expr``: introduces the set ``set`` into the lexical scope of
``the expression expr``::

  let as = { x = "foo"; y = "bar"; };
  in with as; x + y

``with`` can take multiple arguments, it takes the first argument as a
function and applies it to the rest of its arguments and then consumes the set
returned by the function::

  with import <nixpkgs> {};

Set overlay::

  set1 // set2

The resulting set consists of attributes from both set1 and set2. If an
attribute is present in both then set2 overrides set1.

Set operations::
    
    set.attrpath
    set.attrpath or defaultValue
    set ? "attrpath" (does set contain attrpath or not: true/false)
                     (same as: builtins.hasAttr "attrpath" set)
                     (also see "?" in optional args for functions)
    set1 // set2

Set as Function
~~~~~~~~~~~~~~~

A set that has a ``__functor`` attribute whose value is callable (i.e. is
itself a function or a set with a __functor attribute whose value is
callable) can be applied as if it were a function, with the set itself
passed in first::

  let add = { __functor = self: x: x + self.x; };
      inc = add // { x = 1; };
  in inc 1

Functions
~~~~~~~~~

An anonymous function in nix is
defined as ``{ arg1, arg2, ..., argn }: expr`` where ``arg1``, ``arg2``,
and ``argn`` are arguments to the function and ``expr`` is the body of
the function.

Anonymous functions are defined as ``pattern: body``::

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

Calling a function. Whitespace is function application operator::

    # single argument
    f "foo"

    # set argument
    f {x = "foo"; y = "bar"; z = "baz";}

First class functions (functions returning functions)::

    let concat = x: y: x + y; # function returning a function
    in builtins.map (concat "foo") [ "bar" "bla" "abc" ] # Currying

Debugging
~~~~~~~~~

Assertions::

    assert e1; e2

Dynamic
~~~~~~~

Make the set attributes to be accessed, dynamically ::

    let attr = "lib"
    builtins.getAttr attr nixpkgs

Nix Expression Files
--------------------

Any nix expression can be stored in a file and the builtin function ``import``
can be used to load the expression from the file to use it in another
expression. See the documentation of ``import``.

For example, to evaluate an expression from a file::

  $ nix eval '(import ./filename.nix)'

Its common to define a set in a file and use it like this::

  with (import ./definitions.nix); ...

It can also be written as (see ``with``)::

  with import ./definitions.nix; ...

If the file defines a function returning a set, we would have to supply the
function argument to get a set::

  with import <nixpkgs> {};

importing from NIX_PATH
-----------------------

``import <nixpkgs> {};`` searches for ``nixpkgs`` in NIX_PATH and imports it.

Printing on stdout
------------------

You can use ``builtins.trace`` or ``nixpkgs.lib.debug.trace*``
functions.  For example::

  let nixpkgs = import <nixpkgs> {};
  in lib.debug.traceSeq (builtins.attrNames nixpkgs.lib) ""

However, when using ``nix eval`` trace would always return some value,
and any expression evaluation will print that value as well. If we want
to purely print something on terminal and do not want the value of the
expression then we can use ``nix-instantiate --eval`` instead.

Built-in functions
------------------

The easiest way to find top level functions is to use tab in ``nix repl``::

    nix-repl> <tab>

builtins.*
~~~~~~~~~~

Nix provides `a library of built-in functions
<https://nixos.org/nix/manual/#ssec-builtins>`_. All built-in functions
are available through the ``builtins.`` namespace prefix. To see a list of all
builtins::

    nix-repl> builtins.<tab>

builtins.import
~~~~~~~~~~~~~~~

Any nix expression can be stored in a file and the builtin function
``import`` can be used to load the expression from the file to use it in
another expression. ``import filename.nix`` would just be equivalent to
replacing the import statement with the expression in the file.  If the
imported path is a directory, the file ``default.nix`` in that directory
is loaded.

Note that this is different from the import in other languages importing
function definitions from a file.

builtins.derivation
~~~~~~~~~~~~~~~~~~~

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
.................................

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

Functions in nixpkgs
--------------------

The ``nixpkgs`` channel exports a set returning function which takes the
config as an argument.  When importing it we can specify the config::

  let pkgs =
        import <nixpkgs>
            { config.allowBroken = true;
              config.allowUnfree = true;
            }

The returned set consists of library functions and package derivations.

To find more details about any function. Load ``nixpkgs`` in the nix repl and
type the function name::

    nix-repl> :l <nixpkgs>
    nix-repl> stdenv.mkDerivation
    «lambda @ /nix/store/gnkd9i59pswalkflb647fnjjnxgyl1n9-nixpkgs-20.09pre228453.dcb64ea42e6/nixpkgs/pkgs/stdenv/generic/make-derivation.nix:22:5»

Then you can open the file and check the definition.

TBD: scripts to print all functions or all sets in a nixpkgs path, and their
documentations.

Fetchers
~~~~~~~~

nixpkgs.fetch* ::

  nix-repl> :l <nixpkgs>
  nix-repl> fetch<tab>

Trivial Builders
~~~~~~~~~~~~~~~~

The following functions wrap stdenv.mkDerivation, making it easier to
use in certain cases.

Running commands:

* runCommand*

Writing files into nix store:

* write*
* symlinkJoin

* buildFHSUserEnv
* pkgs.mkShell

nixpkgs.lib.*
~~~~~~~~~~~~~

Attribute path ``nixpkgs.lib`` or
``nixpkgs.pkgs.lib`` contains `a library of functions
<https://nixos.org/nixpkgs/manual/#chap-functions>`_ to help in writing
package definitions. To see a list of all::

    nix-repl> :l <nixpkgs>
    nix-repl> lib.<tab>

Some common function sets:

* nixpkgs.lib.asserts.*
* nixpkgs.lib.attrsets.*
* nixpkgs.lib.strings.*
* nixpkgs.lib.trivial.*
* nixpkgs.lib.lists.*
* nixpkgs.lib.debug.*
* nixpkgs.lib.options.*

nixpkgs.stdenv
~~~~~~~~~~~~~~

Attribute path ``nixpkgs.stdenv`` or `nixpkgs.pkgs.stdenv
<https://nixos.org/nixpkgs/manual/#chap-stdenv>`_ contains a nix package that
provides a standard build environment including gcc, GNU coreutils, GNU
findutils and other basic tools::

    $ nix-env -qaP -A nixpkgs.stdenv
    nixpkgs.stdenv  stdenv-darwin

nixpkgs.stdenv.mkDerivation
~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
