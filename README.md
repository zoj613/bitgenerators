# bitgenerators
[![codecov][1]](https://codecov.io/gh/zoj613/bitgenerators)
[![CI][2]](https://github.com/zoj613/bitgenerators/actions/workflows/)
[![license][3]](https://github.com/zoj613/bitgenerators/blob/main/LICENSE)

This library provides a port of numpy's random module bitgenerator interface for working with
pseudo-random number generation. Several PRNG algorithms are implemented and exposed using
numpy's bitgenerator interface in a purely functional manner for Ocaml users.

## Documentation
Comprehensive documentation of available features is available at the project's [site][4].

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
Seq.unfold (fun t -> Some (PCG64.next_double t)) rng |> Seq.take 10 |> List.of_seq
(* - : float list =
   [0.0277127392825169405; 0.90670005548402266; 0.881393554699734239;
    0.624897275420908671; 0.790714811097940395; 0.825908014363094134;
    0.841705835986455209; 0.471727947718599938; 0.95722877984939414;
    0.946591532980609163] *)
```
Supported bitgenerators include: `PCG64`, `Philox4x64`, `Xoshiro256`, `ChaCha` and `SFC64`.

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
