(* Copyright 2014 Melissa O'Neill <oneill@pcg-random.org>
   Copyright 2015 Robert Kern <robert.kern@gmail
   Copyright (c) 2024, Zolisa Bleki

  SPDX-License-Identifier: BSD-3-Clause *)
open Stdint

module U128 = struct
    type t = { high : uint64; low : uint64 }

    let of_u64 high low = {high; low}

    let one = Uint64.{high = zero; low = one}

    let zero = Uint64.{high = zero; low = zero}

    let ( + ) a b =
        match Uint64.{high = a.high + b.high; low = a.low + b.low} with
        | x when x.low < b.low -> {x with high = Uint64.(x.high + one)}
        | x -> x

    let max32 = Uint32.(max_int |> to_uint64)
    let mult64 x y =
        let open Uint64 in
        let x0 = logand max32 x and y0 = logand max32 y
        and x1 = shift_right x 32 and y1 = shift_right y 32 in
        let t = shift_right (x0 * y0) 32 + x1 * y0 in
        {high = shift_right (logand max32 t + x0 * y1) 32 + (shift_right t 32) + x1 * y1; low = x * y}

    let ( * ) a b = match mult64 a.low b.low with
        | {high;low} -> {high = Uint64.(high + a.high * b.low + a.low * b.high); low}

    (* let ( ** ) a b = match mult64 a.low b with
        | x -> {x with high = Uint64.(x.high + a.high * b)} *)
end


module PCG64 : sig
    (** PCG-64 is a 128-bit implementation of O'Neill's permutation congruential
        generator. PCG-64 has a period of {m 2^{128}} and supports advancing an arbitrary
        number of steps as well as {m 2^{127}} streams.

        The specific member of the PCG family that we use is PCG XSL RR 128/64.
        The PCG64 state vector consists of 2 unsigned 128-bit values. One is the
        state of the PRNG, which is advanced by a linear congruential generator (LCG).
        The second is a fixed odd increment used in the LCG.

        The input seed is processed by {!SeedSequence} to generate both values. *)

    include Common.BITGEN

    val advance : uint64 * uint64 -> t -> t
    (** [advance delta] Advances the underlying RNG as if [delta] draws have been made.
        The returned state is that of the generator [delta] steps forward. *)

    val next_bounded_uint64 : uint64 -> t -> uint64 * t
    (** [next_bounded_uint64 bound t] returns an unsigned 64bit integers in the range
        (0, bound) as well as the state of the generator advanced one step forward. *)
end = struct
    type t = {s : setseq; ustore : uint32 option}
    and setseq = {state : U128.t; increment : U128.t}


    let sixtythree = Uint32.of_int32 63l
    let multiplier = U128.of_u64 (Uint64.of_int64 2549297995355413924L)
                                 (Uint64.of_int64 4865540595714422341L)


    (* Uses the XSL-RR output function *)
    let output U128.{high; low} =
      let v = Uint64.(logxor high low) in
      let r = Uint64.(shift_right high 58 |> to_int) in
      let nr = Uint32.(of_int r |> neg |> logand sixtythree |> to_int) in
      Uint64.(logor (shift_left v nr) (shift_right v r))


    let next {state; increment} =
        let state' = U128.(state * multiplier + increment) in
        output state', {state = state'; increment}


    let next_uint64 t = match next t.s with
        | u, s -> u, {t with s}


    let next_uint32 t =
        match Common.next_uint32 ~next:next t.s t.ustore with
        | u, s, ustore -> u, {s; ustore}


    let next_bounded_uint64 bound t = Common.next_bounded_uint64 bound ~nextu64:next_uint64 t


    let next_double t = Common.next_double ~nextu64:next_uint64 t


    let set_seed seed =
      let s2 = Uint64.(logor (shift_left seed.(2) 1) (shift_right seed.(3) 63)) in
      let s3 = Uint64.(logor (shift_left seed.(3) 1) one) in
      let increment = U128.of_u64 s2 s3 in
      let state = U128.(zero * multiplier + increment) in
      {state = U128.((of_u64 seed.(0) seed.(1) + state) * multiplier + increment); increment}


    let advance (d1, d0) {s = {state; increment}; _} =
        let open U128 in
        let half x = U128.{low = Uint64.(logor (shift_right x.low 1) (shift_left x.high 63));
                           high = Uint64.(shift_right x.high 1)} in
        let rec lcg d am ap cm cp =
            match Uint64.(d.high <= zero && d.low <= zero, logand d.low one = one) with
            | true, _ -> am * state + ap
            | false, true -> lcg (half d) (am * cm) (ap * cm + cp) (cm * cm) (cp * (cm + one))
            | false, false -> lcg (half d) am ap (cm * cm) (cp * (cm + one))
        in {s = {state = lcg (of_u64 d1 d0) one zero multiplier increment; increment}; ustore = None}


    let initialize seed =
        {s = set_seed (Seed.SeedSequence.generate_64bit_state 4 seed); ustore = None}
end
