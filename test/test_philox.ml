open OUnit2
open Bitgen
 

let test_philox_datasets _ =
    Testconf.bitgen_groundtruth
        (module Philox4x64)
        (Sys.getcwd () ^ "/../../../test/data/philox-testset-1.csv");
    Testconf.bitgen_groundtruth
        (module Philox4x64)
        (Sys.getcwd () ^ "/../../../test/data/philox-testset-2.csv")


let test_counter_init _ =
    let open Stdint in
    let ss = SeedSequence.initialize [Uint128.of_int 12345] in
    let next_int init =
        Philox4x64.initialize_ctr ~counter:init ss 
        |> Philox4x64.next_uint64 |> fst |> Uint64.to_string
    in
    let base = next_int Uint64.(max_int, max_int, max_int, max_int) in
    assert_bool "" (base <> next_int Uint64.(max_int, max_int, max_int, zero));
    assert_bool "" (base <> next_int Uint64.(max_int, max_int, zero, zero));
    assert_bool "" (base <> next_int Uint64.(max_int, zero, zero, zero));
    assert_bool "" (base <> next_int Uint64.(zero, zero, zero, zero))


let test_jump _ =
    let open Stdint in
    let ss = SeedSequence.initialize [] in
    let t = Philox4x64.initialize_ctr ~counter:Uint64.(max_int, max_int, max_int, max_int) ss |> Philox4x64.jump in
    let t' = Philox4x64.initialize_ctr ~counter:Uint64.(max_int, max_int, zero, max_int) ss |> Philox4x64.jump in
    assert_bool "" ((Philox4x64.next_double t |> fst) <> (Philox4x64.next_double t' |> fst))


let test_advance _ =
    let open Stdint in
    (* since advance uses a 256-bit integer to advance, the equivalent number of
       steps if manually calling next_uint64 would be 4 times larger than the
       steps used to call advance. *)
    let rec advance_n t = function
        | 0 -> t
        | i -> advance_n (Philox4x64.next_uint64 t |> snd) (i - 1) in
    let t = SeedSequence.initialize [Uint128.of_int 12345]
            |> Philox4x64.initialize_ctr ~counter:Uint64.(max_int, max_int, zero, zero) in
    assert_equal
        (Philox4x64.advance Uint64.(of_int 2, zero, zero, zero) t |> Philox4x64.next_uint64 |> fst |> Uint64.to_string)
        (advance_n t (4 * 2) |> Philox4x64.next_uint64 |> fst |> Uint64.to_string)
        ~printer:(fun x -> x)


let tests = [
    "test Philox PNRG against groundtruth data" >:: test_philox_datasets;
    "test behaviour when counter is set" >:: test_counter_init;
    "test Philox jump function consistency" >:: test_jump;
    "test Philox advance function correctness" >:: test_advance;
]
