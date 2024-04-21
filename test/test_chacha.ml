open OUnit2
open Bitgen
open Stdint
 

let test_chacha_datasets _ =
    Testconf.bitgen_groundtruth
        (module ChaCha)
        (Sys.getcwd () ^ "/../../../test/data/chacha-testset-1.csv");
    Testconf.bitgen_groundtruth
        (module ChaCha)
        (Sys.getcwd () ^ "/../../../test/data/chacha-testset-2.csv")


let test_full_init _ =
    let ss = SeedSequence.initialize [Uint128.of_int 12345] in
    let key = SeedSequence.generate_64bit_state 4 ss in
    let full = ChaCha.(initialize_full ~key:key ~counter:Uint64.(max_int, zero) ~rounds:4
               |> next_uint64) |> fst |> Uint64.to_string in
    let default = ChaCha.(initialize ss |> next_uint64) |> fst |> Uint64.to_string in
    assert_bool "stream should not be the same for different init counters" (full <> default)


let test_invalid_round _ =
    let msg = "`rounds` must be a positive, even and > 2" in
    let key = SeedSequence.(initialize [Uint128.of_int 12345] |> generate_64bit_state 4) in
    let ctr = Uint64.(zero, zero) in
    assert_raises (Failure msg) (fun _ -> ChaCha.initialize_full ~key:key ~counter:ctr ~rounds:3);
    (* test non-positive even rounds *)
    assert_raises (Failure msg) (fun _ -> ChaCha.initialize_full ~key:key ~counter:ctr ~rounds:(-10))


let tests = [
    "test ChaCha PRNG against groundtruth data" >:: test_chacha_datasets;
    "test ChaCha full initialization consistency" >:: test_full_init;
    "test if odd number of rounds raises exception" >:: test_invalid_round;
]
