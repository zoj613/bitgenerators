# bitgenerators
[![codecov][1]](https://codecov.io/gh/zoj613/bitgenerators)
[![CI][2]](https://github.com/zoj613/bitgenerators/actions/workflows/)
[![license][3]](https://github.com/zoj613/bitgenerators/blob/main/LICENSE)

This library provides a port of numpy's random module bitgenerator interface for working with
pseudo-random number generation. Several PRNG algorithms are implemented an exposed using
numpy's bitgenerator interface in a purely functional manner for Ocaml users.

## Documentation
Comprehensive documentation of available features is available at the project's [site][4].

## Usage
A `SeedSequence` module based [on these ideas][5] is available to providing a high quality seed sequence that
can be used to initialize any of the supported PRNG's:
```ocaml
open Bitgen

let seedseq = SeedSequence.initialize [] in
let rng = PCG64.initialize seedseq in
```
It can also be used to initialize any custom PRNG using the module's `generate_64bit_state`
and `generate_32bit_state` functions:
```ocaml
open Stdint

SeedSequence.generate_64bit_state 4 seedseq |> Array.map Uint64.to_string 
(* - : string array =
[|"4092832899716182828"; "16750193010238713092"; "6882587689755624013";
  "3060663954516479482"|] *)
```
Below is an example of using an initialized `PCG64` bitgenerator to generate 10 random
floats:
```ocaml
let rec get_floats n t acc = match n <= 0 with
    | true -> List.rev acc, t
    | false -> match PCG64.next_double t with
        | u, t' -> get_floats (n - 1) t' (Float.to_string u :: acc)
in
get_floats 10 rng [] |> fst
(** an example output from the above call would be:
    - : string list =
    ["0.913894299701"; "0.792148446413"; "0.949364012916"; "0.143892066375";
     "0.70297277417"; "0.933940885237"; "0.338831577483"; "0.460820972876";
     "0.61424098101"; "0.38294818093"] *)
```
Supported bitgenerators include: `PCG64`, `Philox64`, `Xoshiro256` and `SFC64`.


[1]: https://codecov.io/gh/zoj613/bitgenerators/graph/badge.svg?token=KOOG2Y1SH5
[2]: https://img.shields.io/github/actions/workflow/status/zoj613/bitgenerators/build-and-test.yml?branch=main
[3]: https://img.shields.io/github/license/zoj613/bitgenerators
[4]: https://zoj613.github.io/bitgenerators/bitgenerators/Bitgen/index.html
[5]: https://www.pcg-random.org/posts/developing-a-seed_seq-alternative.html
