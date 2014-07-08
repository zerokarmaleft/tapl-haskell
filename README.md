# tapl-haskell

Implementations from studing Types and Programming Languages.

## Building

All projects assume you have GHC >=7.8 installed (though with
modification to `.cabal` build files, they'll probably work just fine
on 7.6) and a recent enough version of `cabal-install` to utilize
sandboxes. Then each project can be built with the following:

```
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal build
```

Examples of the implementation language can be found in `/examples` to
test the compiler/REPL.

## License

Copyright © 2014 Edward Cho.

Distributed under the MIT License.
