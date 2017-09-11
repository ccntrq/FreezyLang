# Freezy

A simple interpreted programming language implemented in Haskell.


## Build

`cabal` is used to install the dependencies and build the interpreter

```bash
cabal install
```

## Documentation

This project is exstensivly documented using haddock. To generate the
documentation in html format run

```
cabal haddock
```

Sadly there is a bug with the `--executables` option thus there is no
documentation for `Freezy.hs` at the moment
