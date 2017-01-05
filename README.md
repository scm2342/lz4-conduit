# LZ compression for Data.Conduit

This library uses https://github.com/lz4/lz4.git to implement an lz4 compression conduit in stream mode. Note that the resulting format is custom. The package comes with an executable that is compatible.

# Installation

We provide [cabal] as well as [stack] files.

```bash
$ cabal install lz4-conduit
```

```bash
$ stack build
```

[cabal]: http://www.haskell.org/cbaal
[stack]: https://www.haskellstack.org
