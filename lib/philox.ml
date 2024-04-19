(* Copyright 2010-2012, D. E. Shaw Research. All rights reserved.
   Copyright (c) 2024, Zolisa Bleki

  SPDX-License-Identifier: BSD-3-Clause *)
open Stdint

module Philox : sig
    (** Philox64 is a 64-bit PRNG that uses a counter-based design based on weaker
        (and faster) versions of cryptographic functions. Instances using different
        values of the key produce independent sequences. Philox has a period of
        {m 2^{256} - 1} and supports arbitrary advancing and jumping the sequence
        in increments of {m 2^{128}}. These features allow multiple non-overlapping
        sequences to be generated.

        The Philox state vector consists of a 256-bit value encoded as a 4-element
        unsigned 64-bit tuple and a 128-bit value encoded as a 2-element unsigned
        64-bit tuple. The former is a counter which is incremented by 1 for every
        4 64-bit randoms produced. The second is a key which determined the sequence
        produced. Using different keys produces independent sequences.

        {!SeedSequence} is used to produce a high-quality initial state for the
        key vector. The counter is set to 0.

        The preferred way to use Philox in parallel applications is to use
        the {!SeedSequence.spawn} function to obtain entropy values, and to use these
        to generate new instance of a Philox bitgenerator:
        
        {@ocaml[
            open Bitgen
            let gens =
                SeedSequence.initialize []
                |> SeedSequence.spawn 10
                |> fst
                |> List.map Philox64.initialize
        ]} *)

    include Common.BITGEN

    val initialize_ctr : counter:uint64 * uint64 * uint64 * uint64 -> Seed.SeedSequence.t -> t
    (** Get the initial state of the generator using a 4-element unsigned 64-bit tuple as
        the bitgenerator's [counter] initial state as well as {!SeedSequence.t} for the
        initiale state of the generator's [key].*)

    val jump : t -> t
    (** [jump t] is equivalent to {m 2^{128}} calls to {!Philox64.next_uint64}. *)
end = struct
    type t = {
        key: key;
        ctr : counter;
        buffer : buffer;
        ustore : uint32 Common.store}
    and counter = uint64 * uint64 * uint64 * uint64
    and key = uint64 * uint64
    and buffer = Buffer of int * counter


    let bumpk0 = Uint64.of_string "0x9E3779B97F4A7C15"
    and bumpk1 = Uint64.of_string "0xBB67AE8584CAA73B"
    let bumpkey (x, y) = Uint64.(x + bumpk0, y + bumpk1)


    let mulhilo64 a b =
        let p = Uint128.(of_uint64 a * of_uint64 b) in
        Uint128.(shift_right p 64 |> to_uint64, to_uint64 p)


    let rh0 = Uint64.of_string "0xD2E7470EE14C6C93"
    and rh1 = Uint64.of_string "0xCA5A826395121157"
    let round (c0, c1, c2, c3) (k0, k1) =
        match mulhilo64 rh0 c0, mulhilo64 rh1 c2 with
        | (hi0, lo0), (hi1, lo1) -> Uint64.(logxor hi1 c1 |> logxor k0, lo1,
                                            logxor hi0 c3 |> logxor k1, lo0)


    let ten_rounds ctr key =
        let rec loop r c k = match r with
            | 9 -> round c k
            | i -> 
                let c', k' = round_and_bump c k in
                loop (i + 1) c' k'
        and round_and_bump ctr key = round ctr key, bumpkey key in
        loop 0 ctr key


    let next (c0, c1, c2, c3) =
        let open Uint64 in
        match c0 + one, c1 + one, c2 + one with
        | c0', c1', c2' when (c0' = zero && c1' = zero && c2' = zero) -> (c0', c1', c2', c3 + one)
        | c0', c1', c2' when (c0' = zero && c1' = zero) -> (c0', c1', c2', c3)
        | c0', c1', _ when c0' = zero -> (c0', c1', c2, c3)
        | c0', _, _ -> (c0', c1, c2, c3)


    let index (b0, b1, b2, b3) = function
        | 0 -> b0 | 1 -> b1 | 2 -> b2 | _ -> b3


    let next_uint64 t = match t.buffer with
        | Buffer (i, buf) when i < 4  -> index buf i, {t with buffer = Buffer (i + 1, buf)}
        | _ ->
            let ctr' = next t.ctr in
            let buf = ten_rounds ctr' t.key in
            index buf 0, {t with ctr = ctr'; buffer = Buffer (1, buf)}


    let next_uint32 t =
        match Common.next_uint32 ~next:next_uint64 t t.ustore with
        | u, s, ustore -> u, {s with ustore = ustore}


    let next_double t = Common.next_double ~nextu64:next_uint64 t


    let jump t =
        let c0, c1, c2, c3 = t.ctr in
        let c2' = Uint64.(c2 + one) in
        match Uint64.(c2' = zero) with
        | true -> {t with ctr = (c0, c1, c2', Uint64.(c3 + one))}
        | false -> {t with ctr = (c0, c1, c2', c3)}


    let zeros = Uint64.(zero, zero, zero, zero)


    let initialize_ctr ~counter seed =
        let istate = Seed.SeedSequence.generate_64bit_state 2 seed in
        {ctr = counter;
         ustore = Common.Empty;
         buffer = Buffer (4, zeros);
         key = (istate.(0), istate.(1))}


    let initialize seed = initialize_ctr ~counter:zeros seed
end
