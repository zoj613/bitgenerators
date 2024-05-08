open OUnit2
open Bitgen
 

let test_advance _ =
    let open Stdint in
    let t = SeedSequence.initialize [Uint128.of_int 12345] |> PCG64.initialize in 
    let advance n = Seq.(iterate (fun s -> PCG64.next_uint64 s |> snd) t |> drop n |> uncons |> Option.get |> fst) in
    assert_equal
        (PCG64.advance (Int128.of_int 100) t |> PCG64.next_uint64 |> fst |> Uint64.to_string)
        (advance 100 |> PCG64.next_uint64 |> fst |> Uint64.to_string)
        ~printer:(fun x -> x)


let test_pcg_datasets _ =
    Testconf.bitgen_groundtruth
        (module PCG64)
        (Sys.getcwd () ^ "/../../../test/data/pcg64-testset-1.csv");
    Testconf.bitgen_groundtruth
        (module PCG64)
        (Sys.getcwd () ^ "/../../../test/data/pcg64-testset-2.csv");
    Testconf.bitgen_groundtruth
        (module PCG64DXSM)
        (Sys.getcwd () ^ "/../../../test/data/pcg64dxsm-testset-1.csv");
    Testconf.bitgen_groundtruth
        (module PCG64DXSM)
        (Sys.getcwd () ^ "/../../../test/data/pcg64dxsm-testset-2.csv")


let test_bounded_u64 _ = Testconf.test_bounded_u64 (module PCG64)


let tests = [
    "test PCG64 and PCG64DXSM ran against groundtruth datasets" >:: test_pcg_datasets;
    "test correctness of PCG's advance function" >:: test_advance;
    "test bounded random generation of PCG" >:: test_bounded_u64;
]
