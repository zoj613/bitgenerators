open OUnit2
open Stdint


let maxint32 = Uint64.of_int 0xffffffff
let uint32_of_uint64 x =
  let upper = Uint64.(shift_right x 32 |> to_uint32) in
  let lower = Uint64.(logand maxint32 x |> to_uint32) in
  Uint32.[to_string_hex lower; to_string_hex upper]


let dbl_const = 1.0 /. 9007199254740992.0 
let uniform_of_uint64 x =
  Uint64.(shift_right x 11 |> to_string |> Float.of_string) *. dbl_const |> Float.to_string


module type S = sig
  type t 
  val next_uint64 :  t -> uint64 * t
  val next_uint32 : t -> uint32 * t
  val next_double : t -> float * t
  val next_bounded_uint64: uint64 -> t -> uint64 * t
  val initialize : Bitgen.SeedSequence.t -> t
end


let test_bounded_u64 (module M : S) =
  let open Stdint in
  let is_less bound t = match M.next_bounded_uint64 bound t with
      | u, t' -> Some (u < bound, t') in
  let t = Bitgen.SeedSequence.initialize [Uint128.of_int 12345] |> M.initialize in
  let all_true b = Seq.(unfold (is_less b) t |> take 100 |> fold_left (&&) true) in
  List.iter (fun b -> assert_equal true (all_true b)) Uint64.[of_int 1; of_int 4193609425186963870]


(* This tests the correctness of a bitgenerator's implementation against groundtruth data for a given seed.
   This function takes the module representing the bitgenerator as well as the path to the CSV file
   containing the groundtruth data. The data is sourced from numpy's random module test suite. *)
let bitgen_groundtruth (module M : S) file =
  let u64 t = let u, t' = M.next_uint64 t in Some (Uint64.to_string_hex u, t')
  and u32 t = let u, t' = M.next_uint32 t in Some (Uint32.to_string_hex u, t')
  and double t = let u, t' = M.next_double t in Some (Float.to_string u, t')

  in open_in file |> Csv.load_in |> List.(map tl) |> List.flatten |> function
  | [] -> assert_failure "There was an error parsing the CSV file"
  | seed :: data ->
      let t = Bitgen.SeedSequence.initialize [Uint128.of_string seed] |> M.initialize in
      let n = List.length data in
      assert_equal
        data Seq.(unfold u64 t |> take n |> List.of_seq) ~printer:(List.fold_left (^) "");

      assert_equal
          List.(map Uint64.of_string data |> map uint32_of_uint64 |> concat)
          Seq.(unfold u32 t |> take (n * 2) |> List.of_seq)
          ~printer:(List.fold_left (^) "");

      assert_equal
          List.(map Uint64.of_string data |> map uniform_of_uint64)
          Seq.(unfold double t |> take n |> List.of_seq)
          ~printer:(List.fold_left (^) "");
