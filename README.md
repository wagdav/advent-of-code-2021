My solutions to the programming puzzles in the [Advent of Code
2021](https://adventofcode.com/2021) written in Clojure.

# Build and run

Install the Nix package manager then

```
nix build
```

# Develop

Run all tests:
```
nix develop --command clj -X:test
```

Run linting:

```
nix develop --command clj-kondo --lint .
```
