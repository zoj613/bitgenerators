open OUnit2
open Bitgen
 

let test_efiix64_datasets _ =
    Testconf.bitgen_groundtruth (module EFIIX64x48) (Sys.getcwd () ^ "/../../../test/data/efiix64-testset-1.csv");
    Testconf.bitgen_groundtruth (module EFIIX64x48) (Sys.getcwd () ^ "/../../../test/data/efiix64-testset-2.csv")


let test_bounded_u64 _ = Testconf.test_bounded_u64 (module EFIIX64x48)


let tests = [
    "test EFIIX64x68 PRNG against groundtruth data" >:: test_efiix64_datasets;
    "test bounded random generation of EFIIX64x68" >:: test_bounded_u64;
]
