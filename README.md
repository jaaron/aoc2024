# Advent Of Code 2024

Just working through the Advent of Code in Ocaml. See
https://adventofcode.com/2024 for challenge problems. 

For each day the solution code will be in a single source code file
`bin/dayN.ml`, the example input given in the problem description in
will be in `examples/dayN.txt` with expected output in
`examples/dayN.output`, and the actual puzzle input in
`inputs/dayN.txt`.

If some functions seem to be useful across multiple days (e.g., reused
input formats) they may be added to a `lib/util.ml` or similar.

## Dependencies

In general, the goal is to implement self-contained solution without
significant dependencies. The only consistent exception is the
Janestreet `Core` library that provides a much more featureful
standard library. 

To reduce some boilerplate, I'm also using the `ppx_deriving` package
to generate straightforward functions (e.g., equality) for datatypes.

Additionally, the build process uses the `dune` tool.

Installation is standard (assuming functional `opam` environment):

```
$ opam install core dune ppx_deriving
```

## Compilation and Running

All days can be compiled with

```
$ dune build
```

Solutions all take a single command line argument with the path to the
input file and can be run as:
```
$ dune exec ./bin/dayN.exe ./inputs/dayN.txt
```

The small examples from the problem descriptions can be run as test
cases using:
```
$ dune runtest
```
