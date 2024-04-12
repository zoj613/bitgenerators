open OUnit2
open Bitgen
 

let test_xoshiro_datasets _ =
    Testconf.bitgen_groundtruth
        (module Xoshiro256)
        (Sys.getcwd () ^ "/../../../test/data/xoshiro256-testset-1.csv");
    Testconf.bitgen_groundtruth
        (module Xoshiro256)
        (Sys.getcwd () ^ "/../../../test/data/xoshiro256-testset-2.csv")


let tests = [
    "test Xoshiro256** PRNG against groundtruth datasets" >:: test_xoshiro_datasets
]
