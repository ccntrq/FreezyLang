# Freezy

A simple interpreted programming language implemented in Haskell.

This is a learning/toy project for me. I am mainly using this to teach me some
Haskell and to learn something about programming languages, interpreters and
compilers in general.

A Freezy Program might look like this:

```
fun plus(a, b,) {
    a + b
}

# this is a comment :)

let a = fn (a,b){a+b}(1,4)
print a # prints 5

if plus( 2, 5) > 7
    then "wut"
    else "jay"

let a = 10
let name = "peter"

print name
print a
print name ~ a
```

There are some more examples in the `examples` folder.

## Build

`cabal` is used to install the dependencies and build the interpreter

```bash
cabal install
cabal build
```

## Run

To run the interpreter just execute the generated binary. You can either pass a
file the should be interpreted or simply start the program without any args to
be dropped into rudimentary repl.

## Documentation

This project is extensively documented using haddock. To generate the
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

- [Learn You A Haskell For Great Good](http://learnyouahaskell.com/) by Miran Lipovača
- [Real World Haskell](http://book.realworldhaskell.org/) by Bryan O'Sullivan, Don Stewart, and John Goerzen
- [Crafting Interpreters](http://www.craftinginterpreters.com) by Bob Nystrom

### Papers:

- [Monad Transformers Step by Step](https://page.mi.fu-berlin.de/scravy/realworldhaskell/materialien/monad-transformers-step-by-step.pdf) by Martin Grabmüller

### Other:

- the Haskell Wiki
