(* Copyright (c) 2018, David Blackman and Sebastiano Vigna (vigna@acm.org)
   Copyright (c) 2024, Zolisa Bleki

  SPDX-License-Identifier: BSD-3-Clause *)
open Stdint

module Xoshiro256StarStar : sig
    (** Xoshiro256** is a 64-bit PRNG that uses a carefully constructed linear transformation.
        This produces a fast PRNG with excellent statistical quality. Xoshiro256**
        has a period of {m 2^{256} - 1} and supports jumping the sequence in increments
        of {m 2^{128}}  which allows multiple non-overlapping subsequences to be generated.
        
        The Xoshiro256 state consists of a 4-element tuple of 64-bit unsigned integers.
        Xoshiro256 is seeded using either a vector of 64-bit unsigned integers.
        The {!SeedSequence} module is used to generate the required 4 values as
        initial state.

        Xoshiro256 can be used in parallel applications by calling the
        method {!Xoshiro256.jump} function which advances the state as-if {m 2^{128}}
        random numbers have been generated. This allows the original sequence to be split
        so that distinct segments can be used in each worker process.*)

    include Common.BITGEN

    val jump : t -> t
    (** [jump t] is equivalent to {m 2^{128}} calls to {!Xoshiro256.next_uint64};
        it can be used to generate {m 2^{128}} non-overlapping subsequences for
        parallel computations. *)
end = struct
    type t = {s : state; ustore : uint32 Common.store}
        and state = uint64 * uint64 * uint64 * uint64


    let rotl x k =
        Uint64.shift_right x (64 - k) |> Uint64.(logor (shift_left x k))


    let next (w, x, y, z) =
        let open Uint64 in match logxor y w, logxor z x with
        | y', z' -> of_int 9 * rotl (x * of_int 5) 7,
                    (logxor w z', logxor x y', logxor y' (shift_left x 17), rotl z' 45)


    let next_uint64 t = match next t.s with
        | u, s' -> u, {t with s = s'}


    let next_uint32 t =
        match Common.next_uint32 ~next:next t.s t.ustore with
        | u, s, ustore -> u, {s; ustore} 


    let next_double t = Common.next_double ~nextu64:next_uint64 t


    let jump = Uint64.(
        [| of_int 0x180ec6d33cfd0aba; of_string "0xd5a61266f0c9392c";
           of_string "0xa9582618e03fc9aa"; of_int 0x39abdc4529b1661c |])


    let jump t = 
        let map2 f (x0, x1, x2, x3) (y0, y1, y2, y3) = (f x0 y0, f x1 y1, f x2 y2, f x3 y3) in
        let rec loop b j (acc, st) =
            match b >= 64, Uint64.(logand j (shift_left one b) > zero) with
            | true, _ -> acc, st 
            | false, true -> loop (b + 1) j (map2 Uint64.logxor acc st, (next st |> snd))
            | false, false -> loop (b + 1) j (acc, (next st |> snd))
        in
        {t with s = loop 0 jump.(0) (Uint64.(zero, zero, zero, zero), t.s)
         |> loop 0 jump.(1) |> loop 0 jump.(2) |> loop 0 jump.(3) |> fst}


    let initialize seed =
        let istate = Seed.SeedSequence.generate_64bit_state 4 seed in
        {s = (istate.(0), istate.(1), istate.(2), istate.(3)); ustore = Common.Empty}
end
