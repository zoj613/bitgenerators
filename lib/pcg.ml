(* Copyright 2014 Melissa O'Neill <oneill@pcg-random.org>
   Copyright 2015 Robert Kern <robert.kern@gmail
   Copyright (c) 2024, Zolisa Bleki

  SPDX-License-Identifier: BSD-3-Clause *)
open Stdint


module PCG64 : sig
    (** PCG-64 is a 128-bit implementation of O'Neill's permutation congruential
        generator. PCG-64 has a period of {m 2^{128}} and supports advancing an arbitrary
        number of steps as well as {m 2^{127}} streams.

        The specific member of the PCG family that we use is PCG XSL RR 128/64.
        The PCG64 state vector consists of 2 unsigned 128-bit values. One is the
        state of the PRNG, which is advanced by a linear congruential generator (LCG).
        The second is a fixed odd increment used in the LCG.

        The input seed is processed by {!SeedSequence} to generate both values. *)

    type t
    (** [t] is the state of the PCG64 bitgenerator *)

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

    val advance : int128 -> t -> t
    (** [advance delta] Advances the underlying RNG as if [delta] draws have been made.
        The returned state is that of the generator [delta] steps forward. *)

    val next_bounded_uint64 : uint64 -> t -> uint64 * t
    (** [next_bounded_uint64 bound t] returns an unsigned 64bit integers in the range
        (0, bound) as well as the state of the generator advanced one step forward. *)
end = struct
    type t = {s : setseq; has_uint32 : bool; uinteger : uint32}
    and setseq = {state : uint128; increment : uint128}

    let multiplier = Uint128.of_string "0x2360ed051fc65da44385df649fccf645"

    (* Uses the XSL-RR output function *)
    let output state =
        let v = Uint128.(shift_right state 64 |> logxor state |> Uint64.of_uint128)
        and r = Uint128.(shift_right state 122 |> to_int) in
        let nr = Uint32.(of_int r |> neg |> logand (of_int 63) |> to_int) in
        Uint64.(logor (shift_left v nr) (shift_right v r))


    let next {state; increment} =
        let state' = Uint128.(state * multiplier + increment) in
        output state', {state = state'; increment}


    let next_uint64 t = match next t.s with
        | u, s' -> u, {t with s = s'}
    

    let next_uint32 t = match t.has_uint32 with
        | true -> t.uinteger, {t with has_uint32 = false}
        | false ->
            let uint, s' = next t.s in
            Uint64.(of_int 0xffffffff |> logand uint |> to_uint32),
            {uinteger = Uint64.(shift_right uint 32 |> to_uint32);
             has_uint32 = true;
             s = s'}


    let next_double t = match next_uint64 t with
        | u, t' ->
            Uint64.(shift_right u 11 |> to_int) |> Float.of_int |> ( *. ) (1.0 /. 9007199254740992.0), t'


    let advance delta {s = {state; increment}; has_uint32; uinteger} =
        let open Uint128 in
        let rec lcg d am ap cm cp =  (* advance state using LCG method *)
            match d = zero, logand d one = one with
            | true, _ -> am * state + ap
            | false, true -> lcg (shift_right d 1) (am * cm) (ap * cm + cp) (cm * cm) (cp * (cm + one))
            | false, false -> lcg (shift_right d 1) am ap (cm * cm) (cp * (cm + one))
        in
        {s = {state = lcg (Uint128.of_int128 delta) one zero multiplier increment; increment};
         has_uint32; uinteger}


    let set_seed (shi, slo, ihi, ilo) =
        let open Uint128 in
        let s = logor (shift_left (of_uint64 shi) 64) (of_uint64 slo)
        and i = logor (shift_left (of_uint64 ihi) 64) (of_uint64 ilo) in
        let increment = logor (shift_left i 1) one in
        {state = (increment + s) * multiplier + increment; increment}

    (* https://www.pcg-random.org/posts/bounded-rands.html *)
    let next_bounded_uint64 bound t =
        let rec loop threshold = function
            | r, s when r >= threshold -> s, r
            | _, s -> loop threshold (next s)
        in
        let setseq', r' = loop (Uint64.rem (Uint64.neg bound) bound) (next t.s) in
        Uint64.rem r' bound, {t with s = setseq'} 


    let initialize seed =
        let state = Seed.SeedSequence.generate_64bit_state 4 seed in
        {s = set_seed (state.(0), state.(1), state.(2), state.(3));
         uinteger = Uint32.zero;
         has_uint32 = false}
end
