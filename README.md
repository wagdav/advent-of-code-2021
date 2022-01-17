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
nix shell nixpkgs#clojure --command clj -X:test
```

Run linting:

```
nix shell nixpkgs#clj-kondo --command clj-kondo --lint .
```

```
nix shell nixpkgs#clojure --command clj -M:test:eastwood
```
