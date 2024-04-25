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

    include Common.BITGEN
end = struct
    (* last uint64 value is the counter *)
    type t = {s : state; ustore : uint32 Common.store}
    and state = uint64 * uint64 * uint64 * uint64


    let next (w, x, y, z) = match Uint64.(w + x + z) with
        | u -> u, Uint64.(shift_right x 11 |> logxor x, y + shift_left y 3,
                          logor (shift_left y 24) (shift_right y 40) + u, z + one)


    let next_uint64 t = match next t.s with
        | u, s -> u, {t with s}


    let next_uint32 t =
        match Common.next_uint32 ~next:next t.s t.ustore with
        | u, s, ustore -> u, {s; ustore} 


    let next_double t = Common.next_double ~nextu64:next_uint64 t


    let set_seed (w, x, y) =
        let rec loop s = function
            | 0 -> s
            | i -> loop (next s |> snd) (i - 1)
        in
        loop (w, x, y, Uint64.one) 12


    let initialize seed =
        let istate = Seed.SeedSequence.generate_64bit_state 3 seed in
        {s = set_seed (istate.(0), istate.(1), istate.(2)); ustore = Common.Empty}
end
