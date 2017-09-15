# Freezy

A simple interpreted programming language implemented in Haskell.

## Build

`cabal` is used to install the dependencies and build the interpreter

```bash
cabal install
cabal build
```

## Documentation

This project is exstensivly documented using haddock. To generate the
documentation in html format run

```
cabal haddock
```

Sadly there is a bug with the `--executables` option thus I moved the
'executable' to lib and added a stupid wrapper as the executable as a
workaround

## Resources

This is a Haksell/Interpreters learning project for me. These are some of the
resources that I used while building this:

### Books:

- Learn You A Haskell For Great Good
- Real World Haskell
- Crafting Interpreters

### Papers:

- Monad Transformers Step by Step

### Other:

- the Haskell Wiki
