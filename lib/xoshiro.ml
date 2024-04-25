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
    and state = uint64 array


    let rotl x k =
        let d = 64 - k in Uint64.(logor (shift_left x k) (shift_right x d))


    let five, nine = Uint64.(of_int 5, of_int 9)

    let next s =
        let open Uint64 in match logxor s.(2) s.(0), logxor s.(3) s.(1) with
        | y', z' -> nine * rotl (s.(1) * five) 7,
                    [|logxor s.(0) z'; logxor s.(1) y'; logxor y' (shift_left s.(1) 17); rotl z' 45|]


    let next_uint64 t = match next t.s with
        | u, s' -> u, {t with s = s'}


    let next_uint32 t =
        match Common.next_uint32 ~next:next t.s t.ustore with
        | u, s, ustore -> u, {s; ustore} 


    let next_double t = Common.next_double ~nextu64:next_uint64 t


    let jump = Uint64.(
        [| of_int 0x180ec6d33cfd0aba; of_string "0xd5a61266f0c9392c";
           of_string "0xa9582618e03fc9aa"; of_int 0x39abdc4529b1661c |])
    let zeros = Uint64.[|zero; zero; zero; zero|]

    let jump t = 
        let rec loop b j (acc0, acc1) =
            match b >= 64, Uint64.(logand j (shift_left one b) > zero) with
            | true, _ -> acc0, acc1 
            | false, true -> loop (b + 1) j (Array.map2 Uint64.logxor acc0 acc1, (next acc1 |> snd))
            | false, false -> loop (b + 1) j (acc0, (next acc1 |> snd))
        in {t with s = Array.fold_right (loop 0) jump (zeros, t.s) |> fst}


    let initialize seed =
        {s = Seed.SeedSequence.generate_64bit_state 4 seed; ustore = Common.Empty}
end
