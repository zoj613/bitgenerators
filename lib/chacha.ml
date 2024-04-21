(* Copyright (c) 2015 Orson Peters <orsonpeters@gmail.com>
   Copyright (c) 2019, Kevin Sheppard. All rights reserved.
   Copyright (c) 2024, Zolisa Bleki

   SPDX-License-Identifier: BSD-3-Clause
    
   Modifications
   =============
   2015: Initial C++ implementation by Orson Peters (https://gist.github.com/orlp/32f5d1b631ab092608b1).
   2019: Ported to C99 by Kevin Sherppard for the `randomgen` python package. (https://github.com/bashtage/randomgen/blob/main/randomgen/src/chacha/chacha.h)
   2024: Ported to OCaml by Zolisa Bleki. *)
open Stdint


module ChaCha128Counter : sig
    (** ChaCha is a 64-bit PRNG that uses a counter-based design based on
        the ChaCha cipher. Instances using different values of the key produce
        sequences. ChaCha has a period of {m 2^{128}} and supports arbitrary
        advancing and jumping the sequence in increments of {m 2^{64}}. These
        features allow multiple non-overlapping sequences to be generated.

        The ChaCha state vector consists of a 16-element array of uint32 that
        capture buffered draws from the distribution, an 8-element array of uint32s
        holding the seed, and a 2-element array of uint64 that holds the 128-bit
        counter ([low, high]). The elements of the seed are the value provided by
        the user. Typical values for number of rounds are  4, 8, 12, or 20
        (for high security).

        ChaCha is seeded using a vector of 4 64-bit unsigned integers. By default
        this is provided by {!SeedSequence.generate_64bit_state}. *)

    include Common.BITGEN

    val initialize_full : Seed.SeedSequence.t -> uint64 * uint64 -> int -> t
    (** [initialize_full seedseq counter rounds] initializes the state of the ChaCha
        bitgenerator; where [seedseq] is a {!SeedSequence.t} used to initialize the
        PRNG's key array, [counter] is a 2-tuple used to initialize the 128-bit counter,
        and [rounds] is the number of rounds to use. [rounds] must be non-negative, even
        and greater than 2, else an [Invalid_argument] exception is raised. *)

    val advance : uint128 -> t -> t
    (** [advance n] Advances the generator forward as if [n] calls to {!ChaCha.next_uint32}
        have been made, and returns the new advanced state. *)

end = struct
    type t = {rounds: int; block : uint32 array; keysetup : uint32 array; ctr : uint64 * uint64}


    let rotl32 x n =
        let y = 32 - n in
        let open Uint32 in logor (shift_left x n) (shift_right x y)
 

    let qround e =
        let open Uint32 in
        let f n (a, b, c, d) = let a' = a + b in a', b, c, rotl32 (logxor a' d) n
        and g n (a, b, c, d) = let c' = c + d in a, rotl32 (logxor c' b) n, c', d in
        e |> f 16 |> g 12 |> f 8 |> g 7


    let full_quarter state (i, j, k, l) =
        let w, x, y, z = qround (state.(i), state.(j), state.(k), state.(l)) in
        Array.mapi
            (fun idx v -> match idx with
            | e when e = i -> w | e when e = j -> x
            | e when e = k -> y | e when e = l -> z | _ -> v) state

    
    let indices = [(0, 4, 8, 12); (1, 5, 9, 13); (2, 6, 10, 14); (3, 7, 11, 15);
                    (0, 5, 10, 15); (1, 6, 11, 12); (2, 7, 8, 13); (3, 4, 9, 14)]
    

    let core state n =
        let f s = List.fold_left (fun acc idx -> full_quarter acc idx) s indices in
        let rec loop state0 = function
            | 0 -> state0
            | i -> loop (f state0) (i - 1)
        in
        loop state n


    let mask = Uint32.(max_int |> to_uint64)
    and sixteen32, sixteen64 = Uint32.of_int 16, Uint64.of_int 16
    and constants = Uint32.[| of_int 0x61707865; of_int 0x3320646e;
                              of_int 0x79622d32; of_int 0x6b206574 |]


    let generate_block ctr keysetup rounds =
        let open Uint64 in
        let ctr0, ctr1 = ctr in
        let f x = shift_right x 4 |> logand mask |> to_uint32 in
        let g x = shift_right (shift_right x 4) 32 |> to_uint32 in
        let h x = Uint32.(shift_left (sixteen32 |> rem (of_uint64 x)) 28) in
        let state = [| constants.(0); constants.(1); constants.(2); constants.(3);
                       keysetup.(0); keysetup.(1); keysetup.(2); keysetup.(3);
                       keysetup.(4); keysetup.(5); keysetup.(6); keysetup.(7);
                       f ctr0; Uint32.logor (g ctr0) (h ctr1); f ctr1; g ctr1 |] in
        rounds lsr 1 |> core state |> Array.map2 Uint32.add state


    let next_uint32 t =
        let open Uint64 in
        let idx, t' = match rem (fst t.ctr) sixteen64 with
            (* this branch is unlikely *)
            | i when i = zero -> i, {t with block = generate_block t.ctr t.keysetup t.rounds}
            | i -> i, t
        in
        match t'.block.(to_int idx), fst t.ctr |> add one with
        | u, v when v = zero -> u, {t' with ctr = v, snd t.ctr + one}
        | u, v -> u, {t' with ctr = v, snd t.ctr}

 
    let next_uint64 t =
        let u, t1 = next_uint32 t in
        let v, t2 = next_uint32 t1 in
        Uint64.(logor (shift_left (of_uint32 v) 32) (of_uint32 u)), t2


    let next_double t = Common.next_double ~nextu64:next_uint64 t


    let advance d t =
        (* Split 128bit [d] into [lower, high] 64-bit integers. *)
        let d0, d1 = Uint128.(rem d (of_uint64 Uint64.max_int) |> to_uint64,
                              shift_right d 64 |> to_uint64) in
        let open Uint64 in
        let ctr0, ctr1 = t.ctr in
        let idx = rem ctr0 sixteen64 in
        let ctr' = match ctr0 + d0 with
            | v when v < ctr0 -> v, ctr1 + d1 + one
            | v -> v, ctr1 + d1
        in
        match (idx + d0 >= sixteen64 || d1 > zero) && (rem (fst ctr') sixteen64 > zero) with
        | true -> {t with block = generate_block ctr' t.keysetup t.rounds; ctr = ctr'}
        | false -> {t with ctr = ctr'}


    let set_seed seed stream ctr rounds =
        let open Uint64 in
        let f x = logand x mask |> to_uint32
        and g x = shift_right x 32 |> to_uint32 in
        let keysetup = [| f seed.(0); g seed.(0); f seed.(1); g seed.(1);
                          f stream.(0); g stream.(0); f stream.(1); g stream.(1) |]
        and ctr' = shift_left (shift_right (fst ctr) 4) 4, snd ctr in
        {block = generate_block ctr' keysetup rounds; ctr; keysetup; rounds}


    let initialize_full seed counter = function
        | r when r <= 2 || r mod 2 <> 0 ->
            raise (Invalid_argument "`rounds` must be a positive, even and > 2")
        | r ->
            let key = Seed.SeedSequence.generate_64bit_state 4 seed in
            set_seed (Array.sub key 0 2) (Array.sub key 2 2) counter r


    let initialize seed = initialize_full seed Uint64.(zero, zero) 4
end
