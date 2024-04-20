open Stdint


type 'a store =
    | Empty
    | Value of 'a


let maxint32 = Uint64.of_int 0xffffffff

(* [to_uint32 nxt s store] advances state [s] and optains a random uint64 integers
   using function [nxt]. The resulting int is split into 2 uint32 integers where
   one of them is returned to the caller while the next is put into [store]. If
   [store] is not empty then the [nxt] is not called and the integer stored in
   [store] is returned and an empty store is returned to the caller. A 3-tuple is
   returned of the form: (new uint32, new state s, new store). *)
let next_uint32 ~next s = function
    | Value x -> x, s, Empty
    | Empty ->
        let uint, s' = next s in
        Uint64.(logand uint maxint32 |> to_uint32), s',
        Value Uint64.(shift_right uint 32 |> to_uint32)


(* [next_double nextu64 t] returns a random float from bitgenerator [t] using
   function [nextu64]  and a new bitgenerator with its internal state advanced forward by a step. *)
let next_double ~nextu64 t = match nextu64 t with
    | u, t' -> Uint64.(shift_right u 11 |> to_int) |> Float.of_int
               |> ( *. ) (1.0 /. 9007199254740992.0), t'


module type BITGEN = sig
    type t 
    (** [t] is the state of the bitgenerator. *)

    val next_uint64 :  t -> uint64 * t
    (** [next_uint64 t] Generates a random unsigned 64-bit integer and a state
        of the generator advanced forward by one step. *)

    val next_uint32 : t -> uint32 * t
    (** [next_uint32 t] Generates a random unsigned 32-bit integer and a state
        of the generator advanced forward by one step. *)

    val next_double : t -> float * t
    (** [next_double t] Generates a random 64 bit float and a state of the
        generator advanced forward by one step. *)

    val initialize : Seed.SeedSequence.t -> t
    (** [initialize s] Returns the initial state of the generator. The random stream
        is determined by the initialization of the seed sequence [s] of {!SeedSequence.t} type. *)
end
