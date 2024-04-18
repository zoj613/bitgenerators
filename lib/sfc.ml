(* Copyright (c) 2018 Melissa E. O'Neill
   Copyright (c) 2024, Zolisa Bleki

  SPDX-License-Identifier: BSD-3-Clause *)
open Stdint

module SFC64 : sig
    (** SFC64 is a 256-bit implementation of Chris Doty-Humphrey's Small Fast
        Chaotic PRNG. It has a few different cycles that one might be on, depending
        on the seed; the expected period will be about {m 2^{255}}.

        It incorporates a 64-bit counter which means that the absolute minimum cycle
        length is {m 2^{64}} and that distinct seeds will not run into each other
        for at least {m 2^{64}} iterations. The SFC64 state vector consists of 4
        unsigned 64-bit values. The last is a 64-bit counter that increments by 1
        each iteration. The input seed is processed by {!SeedSequence} to generate
        the first 3 values, then the algorithm is iterated a small number of times to mix.*)

    type t 
    (** [t] is the state of the SFC64 bitgenerator *)

    val next_uint64 :  t -> uint64 * t
    (** Generate a random unsigned 64-bit integer and return a state of the
        generator advanced by one step forward *)

    val next_uint32 : t -> uint32 * t
    (** Generate a random unsigned 32-bit integer and return a state of the
        generator advanced by one step forward *)

    val next_double : t -> float * t
    (** Generate a random 64 bit float and return a state of the
        generator advanced by one step forward *)

    val initialize : Seed.SeedSequence.t -> t
    (** Get the initial state of the generator using a {!SeedSequence} type as input *)

end = struct
    (* last uint64 value is the counter *)
    type t = {s : state; has_uint32 : bool; uinteger : uint32}
    and state = uint64 * uint64 * uint64 * uint64


    let next (w, x, y, z) =
        let uint = Uint64.(w + x + z)
        and y' = Uint64.(logor (shift_left y 24) (shift_right y 40)) in
        uint, Uint64.(shift_right x 11 |> logxor x, y + shift_left y 3, y' + uint, z + one)


    let next_uint64 t =
        let uint, s' = next t.s in
        uint, {t with s = s'}


    let next_uint32 t =
        match t.has_uint32 with
        | true -> t.uinteger, {t with has_uint32 = false}
        | false -> let uint, s' = next t.s in
            Uint64.(of_int 0xffffffff |> logand uint |> to_uint32), {
                uinteger = Uint64.(shift_right uint 32 |> to_uint32);
                has_uint32 = true;
                s = s'
            }


    let next_double t =
        let uint, t' = next_uint64 t in
        let rnd = Uint64.(shift_right uint 11 |> to_int) |> Float.of_int in
        rnd *. (1.0 /. 9007199254740992.0), t'


    let set_seed (w, x, y) =
        let rec loop s' = function
            | 0 -> s'
            | i -> loop (next s' |> snd) (i - 1)
        in
        loop (w, x, y, Uint64.one) 12


    let initialize seed =
        let istate = Seed.SeedSequence.generate_64bit_state 3 seed in
        {s = set_seed (istate.(0), istate.(1), istate.(2));
         has_uint32 = false; uinteger = Uint32.zero}
end
