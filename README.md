My solutions to the programming puzzles in the [Advent of Code
2021](https://adventofcode.com/2021) written in Clojure.

For other years see:

* [2020](https://github.com/wagdav/advent-of-code-2020)
* [2021](https://github.com/wagdav/advent-of-code-2021)
* [2022](https://github.com/wagdav/advent-of-code-2022)

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

```
nix develop --command clj -M:test:eastwood
```
