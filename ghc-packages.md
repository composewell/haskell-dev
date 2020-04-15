## GHC Package Management

When running `ghc` or `ghci` from command line you can only `import`
Haskell packages that ghc knows about. Haskell Packages must be
installed by a build tool, e.g. `cabal`, before ghc can use them.

For quick resolution of any package related issues, when using `ghc`
directly, you can use `ghc -v` to see how and from where GHC is loading
its packages. Read the following paragraphs for a detailed explanation
of package management in GHC to better understand how it works.

### Package Databases

GHC can pick up packages from a `stack of package
databases`. By default there are two databases known to GHC:

* a `global` database at `<ghc-install-dir>/lib/ghc-<version>/package.conf.d/`
* a `user` database at `$HOME/.ghc/arch-os-version/package.conf.d` or on
  Windows at `C:\Documents And Settings\user\ghc\package.conf.d`

The `GHC_PACKAGE_PATH` environment variable and several GHC command line
options can be used to specify a custom stack of package databases. In
addition, environment files can also be used to specify package
databases.

`ghc-pkg` is the utility shipped with `ghc` to create and manipulate package
databases. Usually they are created by build tools.

### What's there in a package database?

A package database directory e.g.
`<ghc-install-dir>/lib/ghc-<version>/package.conf.d/` consists of a
number of `<package>.conf` files or package description file, one for
each installed package.  A `.conf` file describes the package name,
version etc. and where its binaries and interface files are installed.

For example,
`<ghc-install-dir>/lib/ghc-<version>/package.conf.d/base-<version>.conf`
tells us that its static or dynamic lib binaries (`libHSbase-4.13.0.0.a`,
`libHSbase-4.13.0.0-ghc8.8.2.dylib`) and interface files
(`.hi`) of the modules exposed by `base` are installed in
`<ghc-install-dir>/lib/ghc-<version>/base-<version>`.

### Which installed package will be used?

Custom package databases can be created by build tools. Package
databases can be described by package environment files or by
`GHC_PACKAGE_PATH` or by GHC command line options. Note that package
environment files are only available from GHC 8.2.1 onwards.

GHC searches for package databases in the following order, first one is the
highest preference:

1. `-package-env`, `-package-db` etc. command line options
2. `GHC_ENVIRONMENT` environment variable
3. `.ghc.environment.arch-os-version` in the current dir or in any parent dir
4. `$HOME/.ghc/arch-os-version/environments/default`
5. `GHC_PACKAGE_PATH`
6. default `user` (`$HOME/.ghc/arch-os-version/package.conf.d`) and
   `global` (`<ghc-install-dir>/lib/ghc-<version>/package.conf.d/`) package
   databases

`ghc -v` can tell in what order and which package databases GHC is
loading and because of which configuration. If GHC is being invoked by
a build tool like `cabal` then you may want to use the tool's verbosity
option to see what command line options are being passed to GHC. You can
also pass `-v` as an additional GHC option via the build tool to see
what `ghc` is doing.

Note that `ghc-pkg` does not use the environments files therefore the
list of packages reported by `ghc-pkg list` may not contain the packages
used by GHC.

### Where is my package installed?

When you use `cabal update; cabal install streamly --lib` what happens? `cabal
update` downloads the latest index of packages from `hackage`, the index can be
found at `~/.cabal/packages/hackage.haskell.org/01-index.tar.gz`.

`cabal install streamly --lib` downloads the source tarball of latest or
requested version of `streamly` from hackage and places it in, say,
`~/.cabal/packages/hackage.haskell.org/streamly/0.7.1/`. It then untars
the source, builds it and installs the resulting binaries and interface
files in, say, `~/.cabal/store/ghc-8.8.2/strmly-0.7.1-2409d7f6`.
It then registers the package in its package database
at `~/.cabal/store/ghc-8.8.2/package.db`, creating a
`~/.cabal/store/ghc-8.8.2/package.db/strmly-0.7.1-2409d7f6.conf` file:

```
name:                 streamly
version:              0.7.1
visibility:           public
id:                   strmly-0.7.1-2409d7f6
key:                  strmly-0.7.1-2409d7f6
...
```

We now have a package with a unique `package-id` `strmly-0.7.1-2409d7f6` in
cabal's package db. cabal's package db is registered in GHC's default
environment file found at `~/.ghc/x86_64-darwin-8.8.2/environments/default`:

```
clear-package-db
global-package-db
package-db /Users/harendra/.cabal/store/ghc-8.8.2/package.db
...
package-id strmly-0.7.1-2409d7f6
```

`cabal install` also adds `package-id strmly-0.7.1-2409d7f6` in this
file so that the package becomes available to GHC by default. Note that
all the packages in cabal's package db are available to GHC but only
those which are listed with a `package-id` entry are visible to it by
default others remain hidden. Hidden packages can also be used by using
an explicit `-package` flag on ghc command line.

### What all packages are available to me?

You can list the packages in global and user package dbs using `ghc-pkg list`.
However, there may be more custom package dbs being used via an environment
file, e.g. in the previous section we saw the cabal package-db
`/Users/harendra/.cabal/store/ghc-8.8.2/package.db` being used in the
default environment file. We can list the packages available from this package
db by using `ghc-pkg list --package-db
/Users/harendra/.cabal/store/ghc-8.8.2/package.db`:

```
    ...
    streamly-0.7.0
    streamly-0.7.1
    streamly-0.7.1
    ...
```

We see that there are multiple packages with the same name and version
available. We can use `ghc-pkg list -v` to find out the package-ids:

```
    ...
    streamly-0.7.0 (strmly-0.7.0-2235c7de)
    streamly-0.7.1 (strmly-0.7.1-2409d7f6)
    streamly-0.7.1 (strmly-0.7.1-56b85fb3)
    ...
```

### Multiple instances of a package

We can have multiple packages with the same name and even same version
in the same package db but having different package-ids.

We saw earlier that cabal created a package-id `strmly-0.7.1-2409d7f6`
for streamly.  The last part in that name is a hash to make the
package-id unique. For example, we can have another instance of
streamly-0.7.1 installed as package-id `strmly-0.7.1-56b85fb3`.

Since only package-id has to be unique you may have multiple packages with the
same name and even same version visible to ghc at the same time. Also, you may
a module of the same name exposed by multiple different packages. In such cases
when you import a module ghc cannot determine where to import it from. For
example if you have the following entries in the environment file:

```
...
package-id strmly-0.7.0-2235c7de
package-id strmly-0.7.1-56b85fb3
...
```

```
$ ghci
GHCi, version 8.8.2: https://www.haskell.org/ghc/  :? for help
Loaded package environment from /Users/harendra/.ghc/x86_64-darwin-8.8.2/environments/default
Prelude> import Streamly

<no location info>: error:
    Ambiguous module name ‘Streamly’:
      it was found in multiple packages: streamly-0.7.0 streamly-0.7.1
Prelude> 
```

### Using packages by package-id

Existence of multiple packages with the same name and version is not an
issue, we can tell ghc which package-id to use, e.g.:

```
$ ghci -package-id strmly-0.7.1-56b85fb3
GHCi, version 8.8.2: https://www.haskell.org/ghc/  :? for help
Loaded package environment from /Users/harendra/.ghc/x86_64-darwin-8.8.2/environments/default
Prelude> import Streamly
Prelude Streamly> 
```

### Uninstalling a package

We can remove the package with package-id `strmly-0.7.1-2409d7f6` from the
previous example using:

```
$ ghc-pkg unregister strmly-0.7.1-2409d7f6 --ipid --package-db /Users/harendra/.cabal/store/ghc-8.8.2/package.db
```

We can see that `strmly-0.7.1-2409d7f6.conf` goes away after this command. But
the installed package artifacts still remain in
`~/.cabal/store/ghc-8.8.2/strmly-0.7.1-2409d7f6/`. We can remove it manually
otherwise cabal will get confused when we try to install the package again.

### Renaming modules

If module names exported by different packages conflict we can rename modules
e.g.

```
$ cat rename.hs
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Data.Stream as S

main = do
    x <- S.fold FL.sum $ S.fromList [1..10]
    print x

$ ghc -package "streamly (Streamly.Prelude as Streamly.Data.Stream)" rename.hs
```

### Packages in Cabal Projects

When building a project, build tools like `cabal` may install packages
in a private `package database` and pass specific options to GHC to tell
it where the packages are located. So the packages that are visible
inside a project may not necessarily be available to `ghc` when invoked
from command line outside the project and vice-versa. The details may
depend on the specific build tool.

Use `cabal build --write-ghc-environment-files=always` to write a
`.ghc.environment.x86_64-darwin-8.8.2` file when cabal builds the
project. If we use `always` this options needs to be used only once, cabal then
remembers it in its global configuration and will always generate the
environment file on each build.

The environment file allows `ghc` to be be able to use all the
package dependencies of the project when invoked from any directory
within the project. This allows compilation of any .hs file within the
project using `ghc` directly.

If we look at the environment file we will see:

```
clear-package-db
global-package-db
package-db /Users/harendra/.cabal/store/ghc-8.8.2/package.db
package-db dist-newstyle/packagedb/ghc-8.8.2
...
package-id fusion-plugin-0.2.0-inplace
package-id streamly-0.7.1-inplace
...
```

It contains cabal's global package-db
`/Users/harendra/.cabal/store/ghc-8.8.2/package.db` and a project
local package-db `dist-newstyle/packagedb/ghc-8.8.2`.  It also lists
package-ids of all the dependencies of the project.  We can use
`ghc-pkg list` to see the packages in these dbs. The local package-db
contains all the source packages that are either defined within the
project or specified in the cabal.project file under `packages` or
`source-repository-package` stanzas.

For more details on cabal projects see [cabal user
guide](https://www.haskell.org/cabal/users-guide/) .

### Boot Packages

GHC compiler code itself depends on a number of packages called `boot
packages`. Some of these packages are automatically installed when you
install `ghc`. `wired-in` packages are a subset of boot packages whose
version is strictly determined by the ghc version e.g. `ghc-prim`,
`base` and `template-haskell`. If you are using the GHC API i.e. the
`ghc` package then it automatically constrains that the versions of
boot packages used by the user package are the same as used by that GHC
version.

### References

See [GHC package database](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/packages.html) for more details on how GHC handles packages.
