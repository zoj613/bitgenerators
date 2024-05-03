open OUnit2
open Bitgen


let test_lxm_datasets _ =
    Testconf.bitgen_groundtruth (module LXM) (Sys.getcwd () ^ "/../../../test/data/lxm-testset-1.csv");
    Testconf.bitgen_groundtruth (module LXM) (Sys.getcwd () ^ "/../../../test/data/lxm-testset-2.csv")


let test_bounded_u64 _ = Testconf.test_bounded_u64 (module LXM)


let test_jump _ =
    let t = SeedSequence.initialize [] |> LXM.initialize in
    let t' = LXM.jump t in
    assert_bool "" ((LXM.next_uint64 t |> fst) <> (LXM.next_uint64 t' |> fst))


let tests = [
    "test LXM PRNG against groundtruth datasets" >:: test_lxm_datasets;
    "test LXM jump function consistency" >:: test_jump;
    "test bounded random generation of LXM" >:: test_bounded_u64;
]
