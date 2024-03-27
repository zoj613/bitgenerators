open OUnit2
open Bitgen
 

let test_pcg_datasets _ =
    Testconf.bitgen_groundtruth
        (module PCG64)
        (Sys.getcwd () ^ "/../../../test/data/pcg64-testset-1.csv");
    Testconf.bitgen_groundtruth
        (module PCG64)
        (Sys.getcwd () ^ "/../../../test/data/pcg64-testset-2.csv")


let tests = [
    "test PCG64 and PCG64DXSM ran against groundtruth datasets" >:: test_pcg_datasets;
]
