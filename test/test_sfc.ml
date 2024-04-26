open OUnit2
open Bitgen
 

let test_sfc_datasets _ =
    Testconf.bitgen_groundtruth
        (module SFC64)
        (Sys.getcwd () ^ "/../../../test/data/sfc64-testset-1.csv");
    Testconf.bitgen_groundtruth
        (module SFC64)
        (Sys.getcwd () ^ "/../../../test/data/sfc64-testset-2.csv")


let test_bounded_u64 _ = Testconf.test_bounded_u64 (module SFC64)


let tests = [
    "test SFC64's next_uint64 against groundtruth data" >:: test_sfc_datasets;
    "test bounded random generation of SFC64" >:: test_bounded_u64;
]
