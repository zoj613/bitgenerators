# bitgenerators
[![codecov][1]](https://codecov.io/gh/zoj613/bitgenerators)
[![CI][2]](https://github.com/zoj613/bitgenerators/actions/workflows/)
[![license][3]](https://github.com/zoj613/bitgenerators/blob/main/LICENSE)

This library provides a port of numpy's random module bitgenerator interface for working with
pseudo-random number generation. Several PRNG algorithms are implemented and exposed using
numpy's bitgenerator interface in a purely functional manner for Ocaml users.

## Documentation
Comprehensive documentation of available features is available at the project's [site][4].

## Installation
The library can be installed using `opam` with the following commands:
```shell
opam update
opam install bitgenerators
```
Alternatively, one can install the development version using the latest git commit:
```shell
opam pin add bitgenerators git+https://github.com/zoj613/bitgenerators
```

## Usage
A `SeedSequence` module based [on these ideas][5] is available to providing a high quality seed sequence that
can be used to initialize any of the supported PRNG's:
```ocaml
open Bitgen
open Stdint

let seedseq = SeedSequence.initialize [Uint128.of_int 123456789]
let rng = PCG64.initialize seedseq 
```
It can also be used to initialize any custom PRNG using the module's `generate_64bit_state`
and `generate_32bit_state` functions:
```ocaml
SeedSequence.generate_64bit_state 4 seedseq |> Array.map Uint64.to_string 
(* - : string array =
   [|"5976902797103608158"; "11230215241436205182"; "1766494744865860250";
     "7661475472903581292"|] *)
```
Below is an example of using an initialized `PCG64` bitgenerator to generate 10 random
floats:
```ocaml
let rec compute t acc = function
    | 0 -> List.rev acc, t
    | i -> let u, t' = PCG64.next_uint64 t in compute t' (Uint64.to_string u :: acc) (i - 1)
in compute rng [] 10 |> fst
(* - : string list =
   ["511209809126027580"; "16725663875132018381"; "16258841331763118777";
    "11527320112047894150"; "14586113755615299794"; "15235313769381631730";
    "15526732141789886995"; "8701844723981253752"; "17657754321871037678";
    "17461531751233692673"] *)
```
One can use `SeedSequence.spawn` to generate many independent bitgenerators
that can be used in parallel computations in a _reproducible_ manner:
```ocaml
let next_float t = Some (PCG64.next_double t) in
let compute t = Seq.unfold next_float t |> Seq.take 6 |> List.of_seq in
SeedSequence.spawn 4 seedseq
|> fst
|> List.map PCG64.initialize
|> List.map (fun x -> Domain.spawn (fun () -> compute x))
|> List.map Domain.join
(* - : float list list =
[[0.407206833679825242; 0.189731803785485376; 0.564081542309661343;
  0.88682304196963746; 0.45229844727129942; 0.701372920128140565];
 [0.580212874721654503; 0.0892784737148068; 0.511665172650789257;
  0.931271866226736411; 0.928633846357239; 0.173606152636579414];
 [0.171815817392286574; 0.585509477690361213; 0.837844400599859318;
  0.569340928519763145; 0.680737776645169879; 0.620841051213270267];
 [0.736203907003532887; 0.479879743687943505; 0.506036578793879;
  0.596207202439843376; 0.792829648424435; 0.540970530700028429]] *)
```
Another pattern to generate independent _non-overlapping_ instances of a bitgenerator
for parallel applications is to use `jump` or `advance` functions for PRNG's that support such functions:
```ocaml
Philox4x64.initialize seedseq |> Seq.iterate Philox4x64.jump |> Seq.take 5 |> List.of_seq 
(* - : Philox4x64.t list = [<abstr>; <abstr>; <abstr>; <abstr>; <abstr>] *)
```
The resulting list of bitgenerators produce non-overlapping streams of random numbers.

Supported bitgenerators include: `PCG64`, `Philox4x64`, `Xoshiro256`, `ChaCha` and `SFC64`. More coming soon!

## Empirical Randomness Testing
Running the test suite provided by [TestU01][6] on the supported generators is supported.
To build the test executable one needs to run `dune build bin`. To see the available
command options run `dune exec -- crush -help`. Below is a sample output from running
`dune exec -- crush pcg64` to test `PCG64` on the Small Crush test suite:
```shell
$ dune exec -- crush pcg64

========= Summary results of SmallCrush =========

 Version:          TestU01 1.2.3
 Generator:        pcg64
 Number of statistics:  15
 Total CPU time:   00:01:40.64

 All tests were passed
```
## Benchmarks
A utility to compare the performance of each bitgenerator's `next_uint64` function is provided.
To compile the benchmark executor run `dune build bin`, and then run it using `dune exec -- bench`.
Once the benchmark run has completed, a summary table will be displayed in stdout.


[1]: https://codecov.io/gh/zoj613/bitgenerators/graph/badge.svg?token=KOOG2Y1SH5
[2]: https://img.shields.io/github/actions/workflow/status/zoj613/bitgenerators/build-and-test.yml?branch=main
[3]: https://img.shields.io/github/license/zoj613/bitgenerators
[4]: https://zoj613.github.io/bitgenerators/bitgenerators/Bitgen/index.html
[5]: https://www.pcg-random.org/posts/developing-a-seed_seq-alternative.html
[6]: https://www.semanticscholar.org/paper/TestU01%3A-A-C-library-for-empirical-testing-of-L'Ecuyer-Simard/ba61b9f0b400b6a375eca7f7ecdb18ad871fa9e8
