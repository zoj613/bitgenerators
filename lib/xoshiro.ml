(* Copyright (c) 2024, Zolisa Bleki

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

    type t 
    (** [t] is the state of the Xoshiro256** bitgenerator *)

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

    val jump : t -> t
    (** [jump t] is equivalent to {m 2^{128}} calls to {!Xoshiro256.next_uint64};
        it can be used to generate {m 2^{128}} non-overlapping subsequences for
        parallel computations. *)
end = struct

    type t = {s : state; has_uint32 : bool; uinteger : uint32}
        and state = uint64 * uint64 * uint64 * uint64


    let rotl x k =
        64 - k |> Uint64.shift_right x |> Uint64.logor (Uint64.shift_left x k)


    let next ((w, x, y, z) : state) : uint64 * state =
        let open Uint64 in
        let y' = logxor y w in
        let z' = logxor z x in
        of_int 9 * rotl (x * of_int 5) 7,
        (logxor w z', logxor x y', logxor y' (shift_left x 17), rotl z' 45)


    let next_uint64 t =
        let u, s' = next t.s in
        u, {t with s = s'}


    let next_uint32 t =
        match t.has_uint32 with
        | true -> t.uinteger, {t with has_uint32 = false}
        | false ->
            let uint, s' = next t.s in
            Uint64.(of_int 0xffffffff |> logand uint |> to_uint32),
            {uinteger = Uint64.(shift_right uint 32 |> to_uint32);
             has_uint32 = true;
             s = s'}


    let next_double t =
        let uint, t' = next_uint64 t in
        Uint64.shift_right uint 11
        |> Uint64.to_string
        |> Float.of_string
        |> Float.mul (1.0 /. 9007199254740992.0), t'


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
        {s = (istate.(0), istate.(1), istate.(2), istate.(3));
         has_uint32 = false;
         uinteger = Uint32.zero}
end
