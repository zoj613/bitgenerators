(* Copyright (c) 2015 Melissa E. O'Neill
   Copyright (c) 2019 NumPy Developers
   Copyright (c) 2024, Zolisa Bleki

  SPDX-License-Identifier: BSD-3-Clause *)
open Stdint


let xshift = Ctypes.sizeof Ctypes_static.uint32_t * 8 / 2
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


let is_empty l = List.compare_length_with l 0 = 0


let mask32 = Uint128.of_int 0xffffffff
(* Convert an unsigned 128bit integer into a list of unsigned 32 bit integers *)
let u128_to_u32_list n =
    let open Uint128 in
    let rec loop acc = function
        | i when i = zero && is_empty acc -> [Uint32.zero]
        | i when i = zero -> List.rev acc
        | i -> loop (Uint32.of_uint128 (logand i mask32) :: acc) (shift_right i 32)
    in loop [] n


let to_u32_array x = List.concat_map u128_to_u32_list x |> Array.of_list


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

    type t = {
        entropy : uint128 list;
        spawn_key : uint128 list;
        children_spawned : int;
        pool : uint32 array;
    }

    let entropy t = t.entropy

    let children_spawned t = t.children_spawned


    let mix_entropy entropy pool_size =
        (* Add in entropy up to the pool size. *)
        let values, leftover = match Array.length entropy, pool_size with
            | le, lp when le > lp -> entropy, le - lp
            | le, lp -> Array.append entropy (Array.init (lp - le) (fun _ -> Uint32.zero)), 0 in
        let hash, pool = Array.fold_left_map hashmix (Uint32.of_int 0x43b0d7e5) values in

        (* mix all bits together so later bits can affect earlier bits *)
        let indices = Array.init pool_size (fun x -> x) in
        let f (acc0, acc1) j = Array.fold_left_map
            (fun c i -> if i <> j then mixhashmix acc1.(j) acc1.(i) c else c, acc1.(i)) acc0 indices in
        let hash', pool' = Array.fold_left f (hash, pool) indices in

        (* Add any remaining entropy, mixing each new entropy word with each pool word. *)
        if leftover > 0 then
            let leftover_indices = (Array.init leftover (fun i -> pool_size + i)) in
            let f (acc0, acc1) j = Array.fold_left_map
                (fun c i -> mixhashmix entropy.(j) acc1.(i) c) acc0 indices in
            Array.fold_left f (hash', pool') leftover_indices |> snd
        else pool'


    let assembled_entropy entropy spawn_key pool_size =
        let run_entropy = to_u32_array entropy
        and spawn_entropy = to_u32_array spawn_key in
        let l = match Array.(length spawn_entropy > 0 && length run_entropy < pool_size) with
            | true -> Array.init (pool_size - Array.length run_entropy) (fun _ -> Uint32.zero)
            | false -> [||]
        in Array.concat [run_entropy; l; spawn_entropy]


    let initialize ?(spawn_key=[]) ?(pool_size=4) entropy =
        (* 128 bits of system entropy are used when entropy is not provided *)
        let entropy' = if is_empty entropy then [randbits128 ()] else entropy in
        let assembled = assembled_entropy entropy' spawn_key pool_size in
        {entropy = entropy'; pool = mix_entropy assembled pool_size;
         spawn_key = spawn_key; children_spawned = 0}


    let spawn n t =
        let f acc i =
            let psize = Array.length t.pool
            and spawn_key = t.spawn_key @ [Uint128.of_int i] in
            {t with spawn_key; children_spawned = 0;
             pool = mix_entropy (assembled_entropy t.entropy spawn_key psize) psize} :: acc
        in List.fold_left f [] (List.init n (fun i -> t.children_spawned + i)),
        {t with children_spawned = t.children_spawned + n}


    let init_b, mult_b = Uint32.(of_int 0x8b51f9dd, of_int 0x58f38ded)

    let generate_32bit_state n_words t =
        let next c = Seq.uncons c |> Option.get in
        let rec loop cycle const state = function
            | 0 -> List.rev state |> Array.of_list
            | i ->
                let data0, cycle' = next cycle in
                let data1 = Uint32.(logxor data0 const) in
                let const' = Uint32.(const * mult_b) in
                let data2 = Uint32.(data1 * const') in
                let data3 = Uint32.(logxor (shift_right data2 xshift) data2) in
                loop cycle' const' (data3 :: state) (i - 1)
        in loop (Array.to_seq t.pool |> Seq.cycle) init_b [] n_words

    (* 64 bit state array is obtained by combining elements i and (i + 1)
       of the [o] array to create a 64 bit integer using the formula
       o.(k + 1) << 32 | o.(k), where k = i * 2 for i = 0, 1, ..., N
       for N = length of the [o] array. *)
    let generate_64bit_state n_words t =
        let o = Array.map Uint64.of_uint32 (generate_32bit_state (n_words * 2) t) in
        let f acc i = acc, Uint64.logor o.(i) (Uint64.shift_left o.(i + 1) 32) in
        Array.init n_words (fun i -> i * 2) |> Array.fold_left_map f 0 |> snd
end
