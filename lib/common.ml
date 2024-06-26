open Stdint


let maxint32 = Uint64.of_int 0xffffffff

(* [to_uint32 nxt s store] advances state [s] and optains a random uint64 integers
   using function [nxt]. The resulting int is split into 2 uint32 integers where
   one of them is returned to the caller while the next is put into [store]. If
   [store] is not empty then the [nxt] is not called and the integer stored in
   [store] is returned and an empty store is returned to the caller. A 3-tuple is
   returned of the form: (new uint32, new state s, new store). *)
let next_uint32 ~next s = function
    | Some x -> x, s, None
    | None ->
        let uint, s' = next s in
        Uint64.(logand uint maxint32 |> to_uint32), s',
        Some Uint64.(shift_right uint 32 |> to_uint32)


(* [next_double nextu64 t] returns a random float from bitgenerator [t] using
   function [nextu64]  and a new bitgenerator with its internal state advanced forward by a step. *)
let next_double ~nextu64 t = match nextu64 t with
    | u, t' -> Uint64.(shift_right u 11 |> to_int) |> Float.of_int
               |> ( *. ) (1.0 /. 9007199254740992.0), t'


(* Generate a random uint64 integer in the range [0, b) where [b] is the upper bound.
   This implementation uses Lemire's method. See: https://arxiv.org/abs/1805.10941 *)
let next_bounded_uint64 b ~nextu64 t =
    let b' = Uint128.of_uint64 b
    and r = Uint64.(rem (neg b) b) in
    let rec loop = function
        | m, s when Uint128.to_uint64 m >= r -> Uint128.(shift_right m 64 |> to_uint64), s
        | _, s -> match nextu64 s with
            | x, s' -> loop Uint128.(of_uint64 x * b', s')
    in let x, t' = nextu64 t in
    match Uint128.(of_uint64 x * b') with
    | m when Uint128.(to_uint64 m < b) -> loop (m, t')
    | m -> Uint128.(shift_right m 64 |> to_uint64), t'


module type BITGEN = sig
    type t 
    (** [t] is the state of the bitgenerator. *)

    val next_uint64 :  t -> uint64 * t
    (** [next_uint64 t] Generates a random unsigned 64-bit integer and a state
        of the generator advanced forward by one step. *)

    val next_uint32 : t -> uint32 * t
    (** [next_uint32 t] Generates a random unsigned 32-bit integer and a state
        of the generator advanced forward by one step. *)

    val next_bounded_uint64 : uint64 -> t -> uint64 * t
    (** [next_bounded_uint64 b t] Generates a random unsigned 64-bit integer
        in the interval {m [0, b)}. It returns the integer as well as the state of the
        generator advanced forward. To generate an integer in the range {m [a, b)},
        one should generate an integer in {m [0, b - a)} using [next_bounded_uint64 (b - a) t]
      and then add [a] to the resulting integer to get the output in the desired range. *)

    val next_double : t -> float * t
    (** [next_double t] Generates a random 64 bit float and a state of the
        generator advanced forward by one step. *)

    val initialize : Seed.SeedSequence.t -> t
    (** [initialize s] Returns the initial state of the generator. The random stream
        is determined by the initialization of the seed sequence [s] of {!SeedSequence.t} type. *)
end
