# P4.hs

P4.hs is a Haskell library for the [P4 language](http://p4.org/).

> P4 is a declarative language for expressing how packets are processed by the pipeline of a network forwarding element such as a switch, NIC, router or network function appliance.

## For Developers

### Quickstart
```
# install dependencies..
$ cabal sandbox init
$ cabal install --enable-tests --only-dependencies

# scrape p4lang/p4factory for test input..
$ cd tests
$ ./setup.sh
$ cd ..

# run tests
$ cabal test

# dump AST for dc_example
$ cabal run p4c -- ast-dump tests/p4factory/dc_example/dc_example.i.p4
```
