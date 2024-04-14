open OUnit2
open Stdint


let uint32_of_uint64 x =
    let upper = Uint64.(shift_right x 32 |> to_uint32) in
    let lower = Uint64.(of_int 0xffffffff |> logand x |> to_uint32) in
    Uint32.[to_string_hex lower; to_string_hex upper]


let uniform_of_uint64 x =
     Uint64.(shift_right x 11 |> to_string |> Float.of_string) *. (1.0 /. 9007199254740992.0) |> Float.to_string


module type S = sig
    type t 
    val next_uint64 :  t -> uint64 * t
    val next_uint32 : t -> uint32 * t
    val next_double : t -> float * t
    val initialize : Bitgen.SeedSequence.t -> t
end
(* This tests the correctness of a bitgenerator's implementation against groundtruth data for a given seed.
   This function takes the module representing the bitgenerator as well as the path to the CSV file
   containing the groundtruth data. The data is sourced from numpy's random module test suite. *)
let bitgen_groundtruth (module M : S) file =
    (* Draw a random 64 bit integer n times from Bitgenerator M. *)
    let rec loop i t acc n = match i >= n with
        | true -> List.rev acc
        | false ->
            let u, t' = M.next_uint64 t in
            loop (i + 1) t' (Uint64.to_string_hex u :: acc) n
    in
    (* Draw a random 32 bit integer n times from Bitgenerator M. *)
    let rec loop_u32 i t acc n = match i >= n with
        | true -> List.rev acc
        | false ->
            let u, t' = M.next_uint32 t in
            loop_u32 (i + 1) t' (Uint32.to_string_hex u :: acc) n
    in
    (* Draw a random 64bit float n times from Bitgenerator M. *)
    let rec loopf i t acc n = match i >= n with
        | true -> List.rev acc
        | false ->
            let u, t' = M.next_double t in
            loopf (i + 1) t' (Float.to_string u :: acc) n
    in
    open_in file |> Csv.load_in |> List.(map tl) |> List.flatten |> function
    | [] ->
        assert_failure "There was an error parsing the CSV file"
    | seed :: data ->
        let t = Bitgen.SeedSequence.initialize [Uint128.of_string seed] |> M.initialize
        in
        assert_equal
            data
            (loop 0 t [] (List.length data))
            ~printer:(List.fold_left (^) ""); 
        assert_equal
            (List.map uint32_of_uint64 (List.map Uint64.of_string data) |> List.concat)
            (loop_u32 0 t [] (List.length data * 2))
            ~printer:(List.fold_left (^) "");
        assert_equal
            (List.map uniform_of_uint64 (List.map Uint64.of_string data))
            (loopf 0 t [] (List.length data))
            ~printer:(List.fold_left (^) "");
