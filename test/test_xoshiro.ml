open OUnit2
open Bitgen


let test_xoshiro_datasets _ =
    Testconf.bitgen_groundtruth
        (module Xoshiro256)
        (Sys.getcwd () ^ "/../../../test/data/xoshiro256-testset-1.csv");
    Testconf.bitgen_groundtruth
        (module Xoshiro256)
        (Sys.getcwd () ^ "/../../../test/data/xoshiro256-testset-2.csv")


let test_bounded_u64 _ = Testconf.test_bounded_u64 (module Xoshiro256)


let test_jump _ =
    let ss = SeedSequence.initialize [] in
    let t = Xoshiro256.initialize ss |> Xoshiro256.jump in
    let t' = Xoshiro256.jump t in
    assert_bool "" ((Xoshiro256.next_double t |> fst) <> (Xoshiro256.next_double t' |> fst))


let tests = [
    "test Xoshiro256** PRNG against groundtruth datasets" >:: test_xoshiro_datasets;
    "test Xoshiro256** jump function consistency" >:: test_jump;
    "test bounded random generation of Xoshiro256**" >:: test_bounded_u64;
]
