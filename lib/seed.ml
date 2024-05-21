(* Copyright (c) 2015 Melissa E. O'Neill
   Copyright (c) 2019 NumPy Developers
   Copyright (c) 2024, Zolisa Bleki

  SPDX-License-Identifier: BSD-3-Clause *)
open Stdint


let xshift = Uint32.bits / 2
let mult_a, mult_l, mult_r = Uint32.(of_int 0x931e8875, of_int 0xca01f9dd, of_int 0x4973f715)

let hashmix const value =
    let open Uint32 in
    let const' = const * mult_a in
    let value' = (logxor value const) * const' in
    const', logxor (shift_right value' xshift) value'


let mixhashmix x y const =
    let const', x' = hashmix const x in
    let res = Uint32.(mult_l * y - mult_r * x') in
    const', Uint32.(logxor (shift_right res xshift) res)


let mask32 = Uint32.max_int |> Uint128.of_uint32
(* Convert a list of unsigned 128bit integers into an array of unsigned 32 bit integers
   by splitting each 128bit integer into a sequence of 32bit ints and concatenating the result.*)
let to_u32_array l =
    let open Uint128 in
    Seq.concat_map (fun n -> if n = zero then Seq.return Uint32.zero else
      Seq.unfold (function
      | n when n > zero -> Some (logand n mask32 |> to_uint32, shift_right n 32)
      | _ -> None) n) (List.to_seq l) |> Array.of_seq


(* Return an integer with 128 random bits by combining 2 integers with 64 random bits. *)
let randbits128 () =
    let x, y = Int64.abs (Random.bits64 ()), Int64.abs (Random.bits64 ()) in
    Uint128.(logor (shift_left (of_int64 x) 64) (of_int64 y))


module SeedSequence : sig
    (** SeedSequence mixes sources of entropy in a reproducible way to set the
        initial state for independent and very probably non-overlapping BitGenerators.

        Once the SeedSequence is initialized, one can call the
        {!SeedSequence.generate_64bit_state} or {!SeedSequence.generate_32bit_state}
        functions to get an appropriately sized seed that can be used to initialize
        any of the supported PRNG's. Calling {!SeedSequence.spawn} will create
        [n] SeedSequence's that can be used to seed independent BitGenerators.

        Best practice for achieving reproducible bit streams is to use
        the empty list as an initializing value, and then use {!SeedSequence.entropy}
        to log the entropy for reproducibility:

        {@ocaml[
            open Bitgen
            let ss = SeedSequence.initialize []
            SeedSequence.entropy ss |> List.map Uint128.to_string
            (*: string list = ["152280350332430215596244075920305924447"] *)
        ]} *)

    type t
    (** [t] is the type of a SeedSequence *)

    val initialize : ?spawn_key:uint128 list -> ?pool_size:int -> uint128 list -> t
    (** [initialize seed] Creates a new seed sequence type given a list of unsigned 128-bit
        integers [seed]. [~spawn_key] is an additional source of entropy based on
        the position of type in the tree of such types created with {!SeedSequence.spawn}.
        Typically users need not set this parameter. [~pool_size] is the size of
        the pooled entropy to store. It defaults to the value 4 to give a 128-bit
         entropy pool. *)

    val generate_32bit_state : int -> t -> uint32 array
    (** [generate_32bit_state n t] returns an array of [n] unsigned 32-bit integers for
        seeding a PRNG and generating it's initial 32-bit state. *)

    val generate_64bit_state : int -> t -> uint64 array
    (** [generate_64bit_state n t] returns an array of [n] unsigned 64-bit integers for
        seeding a PRNG and generating it's initial state. *)

    val spawn : int -> t -> t list * t
    (** [spawn n t] creates [n] independent child types of [t] that can be used
        to initialize [n] bitgenerator instances. *)

    val children_spawned : t -> int
    (** The number of children already spawned by this instance of a SeedSequence type. *)

    val entropy : t -> uint128 list
    (** [entropy t] gives a list representing the entropy used to create [t]. It
        can be used as an argument of {!SeedSequence.initialize} to reproduce the
        the bit stream of [t]. *)
end = struct

    type t = {entropy : uint128 list; spawn_key : uint128 list;
              children_spawned : int; pool : uint32 array}

    let entropy t = t.entropy


    let children_spawned t = t.children_spawned


    let assembled_entropy ~entropy ~spawn_key pool_size =
        let run, spawn = to_u32_array entropy, to_u32_array spawn_key in
        if Array.(length spawn > 0 && length run < pool_size) then
            Array.concat [run; Array.make (pool_size - Array.length run) Uint32.zero; spawn]
        else Array.concat [run; spawn]


    let mix ~entropy ~spawn_key pool_size =
        let entropy' = assembled_entropy ~entropy ~spawn_key pool_size in

        (* Add in entropy up to the pool size. *)
        let values, leftover = match Array.length entropy', pool_size with
            | le, lp when le > lp -> entropy', le - lp
            | le, lp -> Array.append entropy' (Array.init (lp - le) (fun _ -> Uint32.zero)), 0 in
        let hash, pool = Array.fold_left_map hashmix (Uint32.of_int32 0x43b0d7e5l) values in

        (* mix all bits together so later bits can affect earlier bits *)
        let indices = Array.init pool_size (fun x -> x) in
        let f (acc0, acc1) j = Array.fold_left_map
            (fun c i -> if i <> j then mixhashmix acc1.(j) acc1.(i) c else c, acc1.(i)) acc0 indices in
        let hash', pool' = Array.fold_left f (hash, pool) indices in

        (* Add any remaining entropy, mixing each new entropy word with each pool word. *)
        if leftover > 0 then
            let leftover_indices = (Array.init leftover (fun i -> pool_size + i)) in
            let f (acc0, acc1) j = Array.fold_left_map
                (fun c i -> mixhashmix entropy'.(j) acc1.(i) c) acc0 indices in
            Array.fold_left f (hash', pool') leftover_indices |> snd
        else pool'


    let initialize ?(spawn_key=[]) ?(pool_size=4) entropy =
        (* 128 bits of system entropy are used when entropy is not provided *)
        let entropy = if List.compare_length_with entropy 0 = 0 then [randbits128 ()] else entropy in
        {spawn_key; entropy; children_spawned = 0; pool = mix pool_size ~entropy ~spawn_key}


    let spawn n t =
        List.init n (fun x -> Uint128.of_int x)
        |> List.map (fun i -> match t.spawn_key @ [i] with
            | spawn_key -> {t with spawn_key; children_spawned = 0;
                            pool = Array.length t.pool |> mix ~entropy:t.entropy ~spawn_key}),
        {t with children_spawned = t.children_spawned + n}


    let init_b, mult_b = Uint32.(of_int 0x8b51f9dd, of_int32 0x58f38dedl)


    let generate_32bit_state n_words t =
        Seq.unfold (fun (seq, acc) -> match Seq.uncons seq, Uint32.(acc * mult_b) with
          | Some (a, seq'), acc' -> 
              let a' = Uint32.(logxor a acc |> mul acc') in
              Some Uint32.(shift_right a' xshift |> logxor a', (seq', acc'))
          | None, _ -> None) (Array.to_seq t.pool |> Seq.cycle |> Seq.take n_words, init_b)
          |> Array.of_seq


    (* 64 bit state array is obtained by combining elements i and (i + 1) of the [o] array
       to create a 64 bit integer using the formula o.(k + 1) << 32 | o.(k), where k = i * 2
       for i = 0, 1, ..., N for N = length of the [o] array. *)
    let generate_64bit_state n_words t =
        let o = generate_32bit_state (n_words * 2) t |> Array.map Uint64.of_uint32 in
        Array.init n_words (fun i -> Uint64.logor o.(i*2) (Uint64.shift_left o.(i*2 + 1) 32))
end
