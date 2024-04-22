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

    val advance : uint64 * uint64 * uint64 * uint64 -> t -> t
    (** [advance n] Advances the generator forward as if [n] draws have been made,
        and returns the new advanced state.*)

end = struct
    type t = {key: key; ctr : counter; buffer : buffer; buffer_pos : int; ustore : uint32 option}
    and counter = uint64 array
    and key = uint64 array
    and buffer = uint64 array

    let rh0, rh1 = Uint128.(of_string "0xD2E7470EE14C6C93", of_string "0xCA5A826395121157")
    let k0, k1 = Uint64.(of_string "0x9E3779B97F4A7C15", of_string "0xBB67AE8584CAA73B")

    let mulhilo64 a b =
        let p = Uint128.(a * of_uint64 b) in
        Uint128.[|shift_right p 64 |> to_uint64; to_uint64 p|]


    let ten_rounds ctr key =
        let rec loop c k r =
            let c' = match mulhilo64 rh0 c.(0), mulhilo64 rh1 c.(2) with
                | x, y -> Uint64.[|logxor y.(0) c.(1) |> logxor k.(0); y.(1);
                                  logxor x.(0) c.(3) |> logxor k.(1); x.(1)|]
            in match r with
            | 0 -> c'
            | i -> loop c' Uint64.[|k.(0) + k0; k.(1) + k1|] (i - 1)
        in loop ctr key 9


    let next c =
        let open Uint64 in
        match c.(0) + one, c.(1) + one, c.(2) + one with
        | c0', c1', c2' when (c0' = zero && c1' = zero && c2' = zero) -> [|c0'; c1'; c2'; c.(3) + one|]
        | c0', c1', c2' when (c0' = zero && c1' = zero) -> [|c0'; c1'; c2'; c.(3)|]
        | c0', c1', _ when c0' = zero -> [|c0'; c1'; c.(2); c.(3)|]
        | c0', _, _ -> [|c0'; c.(1); c.(2); c.(3)|]


    let next_uint64 t = match t.buffer_pos < 4 with
        | true -> t.buffer.(t.buffer_pos), {t with buffer_pos = t.buffer_pos + 1}
        | false ->
            let ctr = next t.ctr in
            let buffer = ten_rounds ctr t.key in
            buffer.(0), {t with ctr; buffer; buffer_pos = 1}


    let next_uint32 t =
        match Common.next_uint32 ~next:next_uint64 t t.ustore with
        | u, s, ustore -> u, {s with ustore}


    let next_double t = Common.next_double ~nextu64:next_uint64 t


    let jump t =
        let c2' = Uint64.(t.ctr.(2) + one) in match Uint64.(c2' = zero) with
        | true -> {t with ctr = [|t.ctr.(0); t.ctr.(1); c2'; Uint64.(t.ctr.(3) + one)|]}
        | false -> {t with ctr = [|t.ctr.(0); t.ctr.(1); c2'; t.ctr.(3)|]}


    let advance (d0, d1, d2, d3) t =
        let aux s x c =
            let x', c' = match Uint64.(x + one), c with
                | v, true when Uint64.(v = zero) -> v, true
                | v, true -> v, false
                | _, false -> x, c
            in
            match Uint64.(x' + s) with
            | v when (v < x' && c' = false) -> v, true
            | v -> v, c'
        in
        let c0', p0 = aux d0 t.ctr.(0) false in
        let c1', p1 = aux d1 t.ctr.(1) p0 in
        let c2', p2 = aux d2 t.ctr.(2) p1 in
        let c3', _ = aux d3 t.ctr.(3) p2 in
        {t with ctr = [|c0'; c1'; c2'; c3'|]}


    let initialize_ctr ~counter:(w, x, y, z) seed =
        {buffer_pos = 4;
         ctr = [|w; x; y; z|];
         ustore = None;
         buffer = Array.make 4 Uint64.zero;
         key = Seed.SeedSequence.generate_64bit_state 2 seed}


    let initialize seed = initialize_ctr ~counter:Uint64.(zero, zero, zero, zero) seed
end
