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


let test_advance _ =
    let open Stdint in
    (* manually advance ChaCha n times. Since Chacha generates 32bit ints,
       we advance manually using next_uint32, else if we used next_uint64 we
       would need to use n/2 steps.*)
    let rec advance_n t = function
        | 0 -> t
        | i -> advance_n (ChaCha.next_uint32 t |> snd) (i - 1)
    in
    let t = ChaCha.initialize_full (SeedSequence.initialize [Uint128.of_int 12345]) Uint64.(max_int, zero) 4 in
    assert_equal
        (ChaCha.advance (Uint128.of_int 1000) t |> ChaCha.next_uint32 |> fst |> Uint32.to_string)
        (advance_n t 1000 |> ChaCha.next_uint32 |> fst |> Uint32.to_string)
        ~printer:(fun x -> x);
    (* Test zero advancing *)
    assert_equal
        (ChaCha.advance Uint128.zero t |> ChaCha.next_uint32 |> fst |> Uint32.to_string)
        (advance_n t 0 |> ChaCha.next_uint32 |> fst |> Uint32.to_string)
        ~printer:(fun x -> x);
    (* Advancing with the largest 128bit integer should not fail *)
    ignore (ChaCha.advance Uint128.max_int t)


let tests = [
    "test ChaCha PRNG against groundtruth data" >:: test_chacha_datasets;
    "test ChaCha full initialization consistency" >:: test_full_init;
    "test if odd number of rounds raises exception" >:: test_invalid_round;
    "test the correctness of ChaCha's advance function" >:: test_advance;
]
