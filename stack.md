# Build tools

Since Haskell is quite an old language, it has quite a rich history around
build tools. There are a variety available these days, some hearkening back
from the old days, and some more recent.

# [Hackage](hackage.haskell.org)

> Hackage is the Haskell community's central package archive of open source
> software. Package authors use it to publish their libraries and programs while
> other Haskell programmers use tools like cabal-install to download and install
> packages (or people get the packages via their distro).  


# [Cabal](https://www.haskell.org/cabal/)

> Cabal is a system for building and packaging Haskell libraries and programs.
> It defines a common interface for package authors and distributors to easily
> build their applications in a portable way. Cabal is part of a larger
> infrastructure for distributing, organizing, and cataloging Haskell libraries
> and programs. 

# [hpack](https://github.com/sol/hpack)

> Hpack is a format for Haskell packages. It is a modern alternative to the
> Cabal package format and follows different design principles.


# [Stack](https://docs.haskellstack.org/)

> Stack is a cross-platform program for developing Haskell projects. It is aimed
> at Haskellers both new and experienced.

Stack is a build tool for Haskell projects. It doesn't supersede `cabal`, but
integrates with it so you no longer have to touch it any longer. It generates
`package.yaml` files which are an invention of
[hpack](https://github.com/sol/hpack). Stack doesn't utilize `hpack` (as far as
I'm aware), but natively supports the `package.yaml` format.

## Stackage

> Stackage is a stable source of Haskell packages. We guarantee that packages
> build consistently and pass tests before generating nightly and Long Term
> Support (LTS) releases.

Stackage downloads all the packages from hackage, checks that they can build and
link together and releases them as a set of compatible packages that are known
to play nicely together.


## Resolvers

Resolvers specify which release of Stackage you want to build your project
against.

## Files

### `stack.yaml`

### `package.yaml`

### `project-name.cabal`

