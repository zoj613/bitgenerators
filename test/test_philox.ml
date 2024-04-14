open OUnit2
open Bitgen
 

let test_philox_datasets _ =
    Testconf.bitgen_groundtruth
        (module Philox64)
        (Sys.getcwd () ^ "/../../../test/data/philox-testset-1.csv");
    Testconf.bitgen_groundtruth
        (module Philox64)
        (Sys.getcwd () ^ "/../../../test/data/philox-testset-2.csv")


let test_counter_init _ =
    let open Stdint in
    let ss = SeedSequence.initialize [Uint128.of_int 12345] in
    let next_int init =
        Philox64.initialize_ctr ~counter:init ss 
        |> Philox64.next_uint64 |> fst |> Uint64.to_string
    in
    let base = next_int Uint64.(max_int, max_int, max_int, max_int) in
    assert_bool "" (base <> next_int Uint64.(max_int, max_int, max_int, zero));
    assert_bool "" (base <> next_int Uint64.(max_int, max_int, zero, zero));
    assert_bool "" (base <> next_int Uint64.(max_int, zero, zero, zero));
    assert_bool "" (base <> next_int Uint64.(zero, zero, zero, zero))


let test_jump _ =
    let open Stdint in
    let ss = SeedSequence.initialize [] in
    let t = Philox64.initialize_ctr ~counter:Uint64.(max_int, max_int, max_int, max_int) ss |> Philox64.jump in
    let t' = Philox64.initialize_ctr ~counter:Uint64.(max_int, max_int, zero, max_int) ss |> Philox64.jump in
    assert_bool "" ((Philox64.next_double t |> fst) <> (Philox64.next_double t' |> fst))


let tests = [
    "test Philox PNRG against groundtruth data" >:: test_philox_datasets;
    "test behaviour when counter is set" >:: test_counter_init;
    "test Philox jump function consistency" >:: test_jump;
]
