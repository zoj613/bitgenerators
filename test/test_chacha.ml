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


let test_bounded_u64 _ = Testconf.test_bounded_u64 (module ChaCha)


let test_full_init _ =
    let ss = SeedSequence.initialize [Uint128.of_int 12345] in
    let full = ChaCha.(initialize_full ss Uint64.(max_int, zero) 4 |> next_uint64)
               |> fst |> Uint64.to_string in
    let default = ChaCha.(initialize ss |> next_uint64) |> fst |> Uint64.to_string in
    assert_bool "stream should not be the same for different init counters" (full <> default)


let test_invalid_round _ =
    let ctr = Uint64.(zero, zero) in
    let msg = "`rounds` must be a positive, even and > 2" in
    let ss = SeedSequence.initialize [Uint128.of_int 12345] in
    assert_raises (Invalid_argument msg) (fun _ -> ChaCha.initialize_full ss ctr 3);
    (* test non-positive even rounds *)
    assert_raises (Invalid_argument msg) (fun _ -> ChaCha.initialize_full ss ctr (-10))


let test_advance _ =
    let open Stdint in
    (* manually advance ChaCha n times. Since Chacha generates 32bit ints,
       we advance manually using next_uint32, else if we used next_uint64 we
       would need to use n/2 steps.*)
    let t = ChaCha.initialize_full (SeedSequence.initialize [Uint128.of_int 12345]) Uint64.(max_int, zero) 4 in
    let advance n = Seq.(iterate (fun s -> ChaCha.next_uint32 s |> snd) t |> drop n |> uncons |> Option.get |> fst) in
    assert_equal
        (ChaCha.advance (Uint128.of_int 100) t |> ChaCha.next_uint32 |> fst |> Uint32.to_string)
        (advance 100 |> ChaCha.next_uint32 |> fst |> Uint32.to_string)
        ~printer:(fun x -> x);
    (* Test zero advancing *)
    assert_equal
        (ChaCha.advance Uint128.zero t |> ChaCha.next_uint32 |> fst |> Uint32.to_string)
        (advance 0 |> ChaCha.next_uint32 |> fst |> Uint32.to_string)
        ~printer:(fun x -> x);
    (* Advancing with the largest 128bit integer should not fail *)
    ignore (ChaCha.advance Uint128.max_int t)


let tests = [
    "test ChaCha PRNG against groundtruth data" >:: test_chacha_datasets;
    "test ChaCha full initialization consistency" >:: test_full_init;
    "test if odd number of rounds raises exception" >:: test_invalid_round;
    "test the correctness of ChaCha's advance function" >:: test_advance;
    "test bounded random generation of ChaCha" >:: test_bounded_u64;
]
