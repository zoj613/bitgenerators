open Stdint

(* Return an integer with 128 random bits by combining 2 integers with 64 random bits. *)
let randbits128 () =
    let x = Int64.abs (Random.bits64 ())
    and y = Int64.abs (Random.bits64 ()) in
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
    let rec loop i acc =
        if i <= Uint128.zero then
            (* reverse the output list to ensure correct order *)
            if List.compare_length_with acc 0 = 0 then [Uint32.zero] else List.rev acc
        else
            let u32 = Uint32.of_uint128 (Uint128.logand i mask32) in
            loop Uint128.(shift_right i 32) (u32 :: acc)
    in
    loop n []


let to_uint32_list x =
    List.(map u128_to_u32_list x |> concat)


module SeedSequence : sig
    type t
    val initialize : ?spawn_key:int list -> ?pool_size:int -> uint128 list -> t
    val generate_32bit_state : int -> t -> uint32 array
    val generate_64bit_state : int -> t -> uint64 array
    val spawn : int -> t -> t list * t
    val children_spawned : t -> int
    val entropy : t -> uint128 list
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
        let rec loop i mixer const =
            if i >= pool_size then
                mixer, const
            else if i < Array.length entropy then
                let v, const' = hashmix entropy.(i) const in
                mixer.(i) <- v;
                loop (i + 1) mixer const'
            else
                let v, const' = hashmix Uint32.zero const in
                mixer.(i) <- v;
                loop (i + 1) mixer const'
        in
        let mixer, const = loop 0 (Array.make pool_size Uint32.zero) init_a
        in
        let rec inner i k mixer const =
            if i >= pool_size then
                mixer, const
            else if i = k then
                inner (i + 1) k mixer const
            else
                let v, const' = hashmix mixer.(k) const in
                mixer.(i) <- mix mixer.(i) v;
                inner (i + 1) k mixer const'
        in
        let rec outer j mixer const =
            if j >= pool_size then
                mixer, const
            else
                let mixer', const' = inner 0 j mixer const in
                outer (j + 1) mixer' const'
        in
        let mixer, const = outer 0 mixer const
        in
        let rec inner i k mixer const =
            if i >= pool_size then
                mixer, const
            else
                let v, const' = hashmix entropy.(k) const in
                mixer.(i) <- mix mixer.(i) v;
                inner (i + 1) k mixer const'
        in
        let outer j mixer const =
            if j >= Array.length entropy then
                mixer, const
            else
                let mixer', const' = inner 0 j mixer const in
                outer (j + 1) mixer' const'
        in
        outer pool_size mixer const |> fst


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
        let rec loop i acc =
            if i >= (t.children_spawned + n) then
                acc
            else
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
        let rec loop i state cycle const =
            if i >= n_words then
                List.rev state |> Array.of_list
            else
                let data, cycle' = next cycle in
                let data = Uint32.(logxor data const) in
                let const = Uint32.(const * mult_b) in
                let data = Uint32.(data * const) in
                let data = Uint32.(logxor (shift_right data xshift) data) in
                loop (i + 1) (data::state) cycle' const
        in
        loop 0 [] (Array.to_seq t.pool |> Seq.cycle) init_b


    let generate_64bit_state n_words t =
        (* 64 bit state array is obtained by combining elements i and (i + 1)
           of the out32 to create a 64 bit integer using the formula
           out32.(k + 1) << 32 | out32.(k), where k = i * 2 for i = 0, 1, ..., N
           for N = length of out32 array. *)
        let o = generate_32bit_state (n_words * 2) t |> Array.map Uint64.of_uint32 in
        let rec to_uint64_array i state =
            if i >= n_words then
                List.rev state |> Array.of_list
            else
                let k = i * 2 in
                let v = Uint64.logor (Uint64.shift_left o.(k + 1) 32) o.(k) in
                to_uint64_array (i + 1) (v::state)
        in
        to_uint64_array 0 []
end
