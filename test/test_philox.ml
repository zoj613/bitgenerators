open OUnit2
open Bitgen
 

let test_philox_datasets _ =
    Testconf.bitgen_groundtruth
        (module Philox)
        (Sys.getcwd () ^ "/../../../test/data/philox-testset-1.csv");
    Testconf.bitgen_groundtruth
        (module Philox)
        (Sys.getcwd () ^ "/../../../test/data/philox-testset-2.csv")


let tests = [
    "test Philox PNRG against groundtruth data" >:: test_philox_datasets
]
