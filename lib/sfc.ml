open Stdint

module SFC64 : sig
    type t 
    val next_uint64 :  t -> uint64 * t
    val next_uint32 : t -> uint32 * t
    val next_double : t -> float * t
    val initialize : Seed.SeedSequence.t -> t
end = struct
    (* last uint64 value is the counter *)
    type t = {s : state; has_uint32 : bool; uinteger : uint32}
    and state = uint64 * uint64 * uint64 * uint64


    let rotate_left value rot =
        Uint64.((-rot) land 63 |> shift_right value |> logor (shift_left value rot))


    let next (w, x, y, z) =
        let uint = Uint64.(w + x + z) in
        uint, Uint64.(shift_right x 11 |> logxor x, y + shift_left y 3,
         rotate_left y 24 + uint, z + one)


    let next_uint64 t =
        let uint, s' = next t.s in
        uint, {t with s = s'}


    let next_uint32 t =
        match t.has_uint32 with
        | true -> t.uinteger, {t with has_uint32 = false}
        | false -> let uint, s' = next t.s in
            Uint64.(of_int 0xffffffff |> logand uint |> to_uint32), {
                uinteger = Uint64.(shift_right uint 32 |> to_uint32);
                has_uint32 = true;
                s = s'
            }


    let next_double t =
        let uint, t' = next_uint64 t in
        let rnd = Uint64.(shift_right uint 11 |> to_int) |> Float.of_int in
        rnd *. (1.0 /. 9007199254740992.0), t'


    let set_seed t (w, x, y) =
        let rec loop s' = function
            | 0 -> s'
            | i -> loop (next s' |> snd) (i - 1) in
        {t with s = loop (w, x, y, Uint64.one) 12}


    let initialize seed =
        let t = {
            s = Uint64.(zero, zero, zero, zero);
            has_uint32 = false;
            uinteger = Uint32.zero
        } in
        let istate = Seed.SeedSequence.generate_64bit_state 3 seed in
        set_seed t (istate.(0), istate.(1), istate.(2))
end
