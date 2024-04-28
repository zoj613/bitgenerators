(* Copyright 2014 Melissa O'Neill <oneill@pcg-random.org>
   Copyright 2015 Robert Kern <robert.kern@gmail
   Copyright (c) 2024, Zolisa Bleki

  SPDX-License-Identifier: BSD-3-Clause *)
open Stdint


module type PCG_TYPE = sig
    (** PCG-64 is a 128-bit implementation of O'Neill's permutation congruential
        generator. PCG-64 has a period of {m 2^{128}} and supports advancing an arbitrary
        number of steps as well as {m 2^{127}} streams.

        The specific member of the PCG family that we use is PCG XSL RR 128/64.
        The PCG64 state vector consists of 2 unsigned 128-bit values. One is the
        state of the PRNG, which is advanced by a linear congruential generator (LCG).
        The second is a fixed odd increment used in the LCG.

        The input seed is processed by {!SeedSequence} to generate both values. *)

    include Common.BITGEN

    val advance : int128 -> t -> t
    (** [advance delta] Advances the underlying RNG as if [delta] draws have been made.
        The returned state is that of the generator [delta] steps forward. *)
end


module type FUNCTOR_SIG = sig
    type t = {s : setseq; ustore : uint32 option}
    and setseq = {state : uint128; increment : uint128}
    val multiplier : uint128
    val output : uint128 -> uint64
    val next : setseq -> uint64 * setseq
end


module PCG64Impl = struct
    type t = {s : setseq; ustore : uint32 option}
    and setseq = {state : uint128; increment : uint128}

    let multiplier = Uint128.of_string "0x2360ed051fc65da44385df649fccf645"
    let sixtythree = Uint32.of_int32 63l

    (* Uses the XSL-RR output function *)
    let output state =
        let v = Uint128.(shift_right state 64 |> logxor state |> to_uint64)
        and r = Uint128.(shift_right state 122 |> to_int) in
        let nr = Uint32.(of_int r |> neg |> logand sixtythree |> to_int) in
        Uint64.(logor (shift_left v nr) (shift_right v r))
        

    let next {state; increment} =
        let state' = Uint128.(state * multiplier + increment) in
        output state', {state = state'; increment}
end


module PCG64DXSMImpl = struct
    type t = {s : setseq; ustore : uint32 option}
    and setseq = {state : uint128; increment : uint128}

    let cheap_multiplier = Uint64.of_string "0xda942042e4dd58b5"
    let multiplier = Uint128.of_uint64 cheap_multiplier 

    let output state =
        let hi0 = Uint128.(shift_right state 64 |> to_uint64) in
        let lo = Uint128.(logor state one |> to_uint64) in
        let hi1 = Uint64.(shift_right hi0 32 |> logxor hi0) in
        let hi2 = Uint64.(hi1 * cheap_multiplier) in
        let hi3 = Uint64.(shift_right hi2 48 |> logxor hi2) in
        let hi4 = Uint64.(hi3 * lo) in
        hi4


    let next {state; increment} =
        output state, {state = Uint128.(state * multiplier + increment); increment}
end


module Make (M : FUNCTOR_SIG) = struct
    include M


    let next_uint64 t = match next t.s with
        | u, s -> u, {t with s}


    let next_uint32 t = match Common.next_uint32 ~next:next t.s t.ustore with
        | u, s, ustore -> u, {s; ustore} 


    let next_double t = Common.next_double ~nextu64:next_uint64 t


    let advance delta {s = {state; increment}; _} =
        let open Uint128 in
        let rec lcg d am ap cm cp =  (* advance state using LCG method *)
            match d = zero, logand d one = one with
            | true, _ -> am * state + ap
            | false, true -> lcg (shift_right d 1) (am * cm) (ap * cm + cp) (cm * cm) (cp * (cm + one))
            | false, false -> lcg (shift_right d 1) am ap (cm * cm) (cp * (cm + one))
        in {s = {state = lcg (Uint128.of_int128 delta) one zero multiplier increment; increment}; ustore = None}


    let set_seed seed =
        let open Uint128 in
        let s = logor (shift_left (of_uint64 seed.(0)) 64) (of_uint64 seed.(1))
        and i = logor (shift_left (of_uint64 seed.(2)) 64) (of_uint64 seed.(3)) in
        let increment = logor (shift_left i 1) one in
        {state = (increment + s) * multiplier + increment; increment}


    let next_bounded_uint64 bound t = Common.next_bounded_uint64 bound ~nextu64:next_uint64 t


    let initialize seed =
        {s = set_seed (Seed.SeedSequence.generate_64bit_state 4 seed); ustore = None}
end


module PCG64DXSM : PCG_TYPE = Make (PCG64DXSMImpl)
module PCG64 : PCG_TYPE = Make (PCG64Impl)
