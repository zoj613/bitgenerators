open OUnit2
open Stdint


(* This tests the correctness of a bitgenerator's implementation against groundtruth data for a given seed.
   This function takes the module representing the bitgenerator as well as the path to the CSV file
   containing the groundtruth data. The data is sourced from numpy's random module test suite. *)
let bitgen_groundtruth (module M : Bitgen.S) file =
    let rec loop i t acc n =
        if i >= n then
            List.rev acc
        else
            let u, t' = M.next_uint64 t in
            loop (i + 1) t' (Uint64.to_string_hex u :: acc) n
    in
    open_in file |> Csv.load_in |> List.(map tl) |> List.flatten |> function
    | [] ->
        assert_failure "There was an error parsing the CSV file"
    | seed :: data' ->
        let ss = Bitgen.SeedSequence.initialize [Uint128.of_string seed] in
        assert_equal
            data'
            (loop 0 (M.initialize ss) [] (List.length data'))
            ~printer:(List.fold_left (^) "")
