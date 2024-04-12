open OUnit2
open Bitgen
 

let test_advance _ =
    let open Stdint in
    (* manually advance PCG64 n times and return the (n+1)'th random value. *)
    let rec advance_n i t n = match i >= n with
        | true -> PCG64.next_uint64 t |> fst |> Uint64.to_string
        | false -> advance_n (i + 1) (PCG64.next_uint64 t |> snd) n
    in
    let t = SeedSequence.initialize [Uint128.of_int 12345] |> PCG64.initialize in 
    assert_equal
        (PCG64.advance (Int128.of_int 12344) t |> PCG64.next_uint64 |> fst |> Uint64.to_string)
        (advance_n 0 t 12344)
        ~printer:(fun x -> x)


let test_bounded_u64 _ =
    let open Stdint in
    let rec loop i t b acc n = match i >= n with
        | true -> List.fold_left ( && ) true (List.rev acc)
        | false ->
            let u, t' = PCG64.next_bounded_uint64 b t in
            loop (i + 1) t' b ((u < b) :: acc) n
    in
    let init seed =
        SeedSequence.initialize [Uint128.of_int seed] |> PCG64.initialize
    in
    List.iter
        (fun b -> assert_equal true (loop 0 (init b) (Uint64.of_int b) [] 1000))
        [1; 100; 1000; 10_0000; 10_000_000]


let test_pcg_datasets _ =
    Testconf.bitgen_groundtruth
        (module PCG64)
        (Sys.getcwd () ^ "/../../../test/data/pcg64-testset-1.csv");
    Testconf.bitgen_groundtruth
        (module PCG64)
        (Sys.getcwd () ^ "/../../../test/data/pcg64-testset-2.csv")


let tests = [
    "test PCG64 and PCG64DXSM ran against groundtruth datasets" >:: test_pcg_datasets;
    "test correctness of PCG's advance function" >:: test_advance;
    "test bounded random generation of PCG" >:: test_bounded_u64;
]
