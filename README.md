# Freezy

A simple interpreted programming language implemented in Haskell.


## Build

```bash
ghc --make -odir .build -hidir .build -dynamic -Wall FreezyLang.hs FreezyLexer.hs Freezy.hs -main-is Freezy -o freezy
```


## Documentation

This project is exstensivly documented using haddock. To generate the
documentation in html format run

```
haddock *.hs --html -o docs
```
