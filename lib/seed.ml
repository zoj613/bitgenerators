(* Copyright (c) 2015 Melissa E. O'Neill
   Copyright (c) 2019 NumPy Developers
   Copyright (c) 2024, Zolisa Bleki

  SPDX-License-Identifier: BSD-3-Clause *)
open Stdint

(* Return an integer with 128 random bits by combining 2 integers with 64 random bits. *)
let randbits128 () =
    let x = Int64.abs (Random.bits64 ()) and y = Int64.abs (Random.bits64 ()) in
    Uint128.(logor (shift_left (of_int64 x) 64) (of_int64 y))


let xshift = Ctypes.sizeof Ctypes_static.uint32_t * 8 / 2
let mult_a = Uint32.of_int 0x931e8875

let hashmix value const =
    let open Uint32 in
    let value = logxor value const
    and const = const * mult_a in
    let value = value * const in
    let value = logxor (shift_right value xshift) value in
    value, const


let mix_mult_l = Uint32.of_int 0xca01f9dd
let mix_mult_r = Uint32.of_int 0x4973f715

let mix x y =
    let res = Uint32.(mix_mult_l * x - mix_mult_r * y) in
    Uint32.(logxor (shift_right res xshift) res)


let mask32 = Uint128.of_int 0xFFFFFFFF
(* Convert an unsigned 128bit integer into a list of unsigned 32 bit integers *)
let u128_to_u32_list n =
    let rec loop i acc = match i = Uint128.zero with
        | true when List.compare_length_with acc 0 = 0 -> [Uint32.zero]
        | true -> List.rev acc
        | false ->
            loop Uint128.(shift_right i 32) (Uint32.of_uint128 (Uint128.logand i mask32) :: acc)
    in
    loop n []


let to_uint32_list x =
    List.(map u128_to_u32_list x |> concat)


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

    val initialize : ?spawn_key:int list -> ?pool_size:int -> uint128 list -> t
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

    let init_a = Uint32.of_int 0x43b0d7e5

    let mix_entropy entropy pool_size = 
        let rec loop i acc const = match i >= pool_size, i < Array.length entropy with
            | true, _ -> List.rev acc |> Array.of_list, const
            | false, true -> (match hashmix entropy.(i) const with
                | v, const' -> loop (i + 1) (v :: acc) const')
            | false, _ -> match hashmix Uint32.zero const with
                | v, const' -> loop (i + 1) (v :: acc) const'
        in
        let rec inner i k mixer const = match i >= pool_size with
            | true -> mixer, const
            | false when i = k -> inner (i + 1) k mixer const
            | false ->
                let v, const' = hashmix mixer.(k) const in
                mixer.(i) <- mix mixer.(i) v;
                inner (i + 1) k mixer const'
        in
        let rec outer j (mixer, const) = match j >= pool_size with
            | true -> mixer, const
            | false -> outer (j + 1) (inner 0 j mixer const)
        in
        let rec inner2 i k mixer const = match i >= pool_size with
            | true -> mixer, const
            | false ->
                let v, const' = hashmix entropy.(k) const in
                mixer.(i) <- mix mixer.(i) v;
                inner2 (i + 1) k mixer const'
        in
        let rec outer2 j (mixer, const) = match j >= Array.length entropy with
            | true -> mixer, const
            | false -> outer2 (j + 1) (inner2 0 j mixer const)
        in
        (loop 0 [] init_a) |> outer 0 |> outer2 pool_size |> fst


    let assembled_entropy entropy spawn_key pool_size =
        let run_entropy = to_uint32_list entropy
        and spawn_entropy = to_uint32_list spawn_key in
        if List.length spawn_entropy > 0 && List.length run_entropy < pool_size then
            let d = pool_size - List.length run_entropy in
            [run_entropy; List.init d (fun _ -> Uint32.zero); spawn_entropy]
            |> List.concat
            |> Array.of_list
        else
            [run_entropy; spawn_entropy] |> List.concat |> Array.of_list


    let initialize ?(spawn_key=[]) ?(pool_size=4) entropy =
        (* 128 bits of system entropy are used when entropy is not provided *)
        let entropy' = match entropy with
            | [] -> [randbits128 ()]
            | v -> v
        and spawn_key' = List.map Uint128.of_int spawn_key in
        let assembled = assembled_entropy entropy' spawn_key' pool_size in
        {entropy = entropy';
         spawn_key = spawn_key';
         pool = mix_entropy assembled pool_size;
         children_spawned = 0}


    let spawn n t =
        let rec loop i acc = match i >= (t.children_spawned + n) with
            | true -> acc
            | false ->
                let spawn_key = t.spawn_key @ [Uint128.of_int i] in
                let pool_size = Array.length t.pool in
                let e = assembled_entropy t.entropy spawn_key pool_size in
                let v = {t with spawn_key = spawn_key;
                         pool = mix_entropy e pool_size;
                         children_spawned = 0} in
                loop (i + 1) (v::acc)
        in
        loop t.children_spawned [],
        {t with children_spawned = t.children_spawned + n}


    let init_b = Uint32.of_int 0x8b51f9dd
    let mult_b = Uint32.of_int 0x58f38ded

    let generate_32bit_state n_words t =
        (* Get next value of a cyclic sequence, aka cycling over a list of values. *)
        let next c = Seq.uncons c |> Option.get in
        let rec loop i state cycle const = match i >= n_words with
            | true -> List.rev state |> Array.of_list
            | false ->
                let data, cycle' = next cycle in
                let data = Uint32.(logxor data const) in
                let const = Uint32.(const * mult_b) in
                let data = Uint32.(data * const) in
                let data = Uint32.(logxor (shift_right data xshift) data) in
                loop (i + 1) (data :: state) cycle' const
        in
        loop 0 [] (Array.to_seq t.pool |> Seq.cycle) init_b


    let generate_64bit_state n_words t =
        (* 64 bit state array is obtained by combining elements i and (i + 1)
           of the [o] array to create a 64 bit integer using the formula
           o.(k + 1) << 32 | o.(k), where k = i * 2 for i = 0, 1, ..., N
           for N = length of the [o] array. *)
        let o = generate_32bit_state (n_words * 2) t |> Array.map Uint64.of_uint32 in
        let rec to_uint64_array i state = match i >= n_words with
            | true -> List.rev state |> Array.of_list
            | false ->
                let k = i * 2 in
                let v = Uint64.logor (Uint64.shift_left o.(k + 1) 32) o.(k) in
                to_uint64_array (i + 1) (v::state)
        in
        to_uint64_array 0 []
end
