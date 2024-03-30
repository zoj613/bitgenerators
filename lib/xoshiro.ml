open Stdint

module Xoshiro256StarStar : sig
    type t 
    val next_uint64 :  t -> uint64 * t
    val next_uint32 : t -> uint32  * t
    val next_double : t -> float * t
    val initialize : Seed.SeedSequence.t -> t
    val jump : t -> t

end = struct

    type t = {s : state; has_uint32 : bool; uinteger : uint32}
        and state = uint64 * uint64 * uint64 * uint64


    let rotl x k =
        64 - k |> Uint64.shift_right x |> Uint64.logor (Uint64.shift_left x k)


    let next ((w, x, y, z) : state) : uint64 * state =
        let open Uint64 in
        let y' = logxor y w in
        let z' = logxor z x in
        of_int 9 * rotl (x * of_int 5) 7,
        (logxor w z', logxor x y', logxor y' (shift_left x 17), rotl z' 45)


    let next_uint64 t =
        let u, s' = next t.s in
        u, {t with s = s'}


    let next_uint32 t =
        match t.has_uint32 with
        | true ->
            t.uinteger, {t with has_uint32 = false}
        | false ->
            let uint, s' = next t.s in
            Uint64.(of_int 0xffffffff |> logand uint |> to_uint32),
            {uinteger = Uint64.(shift_right uint 32 |> to_uint32);
             has_uint32 = true;
             s = s'}


    let next_double t =
        let uint, t' = next_uint64 t in
        Uint64.shift_right uint 11
        |> Uint64.to_string
        |> Float.of_string
        |> Float.mul (1.0 /. 9007199254740992.0), t'


    let jump = Uint64.(
        [| of_int 0x180ec6d33cfd0aba; of_string "0xd5a61266f0c9392c";
           of_string "0xa9582618e03fc9aa"; of_int 0x39abdc4529b1661c |])


    let jump t =
        let rec loop b j acc st = match b >= 64 with
            | true -> acc, st 
            | false ->
                let acc' = match Uint64.(logand j (shift_left one b) > zero) with
                    | true ->
                        let a0, a1, a2, a3 = acc in
                        let s0, s1, s2, s3 = st in
                        Uint64.(logxor a0 s0, logxor a1 s1,
                                logxor a2 s2, logxor a3 s3)
                    | false -> acc
                in
                loop (b + 1) j acc' (next st |> snd)
        in
        let a, s = loop 0 jump.(0) Uint64.(zero, zero, zero, zero) t.s in
        let a, s = loop 0 jump.(1) a s in
        let a, s = loop 0 jump.(2) a s in
        {t with s = loop 0 jump.(3) a s |> fst}


    let initialize seed =
        let t = {s = Uint64.(zero, zero, zero, zero);
                 has_uint32 = false;
                 uinteger = Uint32.zero} in
        let istate = Seed.SeedSequence.generate_64bit_state 4 seed in
        {t with s = (istate.(0), istate.(1), istate.(2), istate.(3))}
end
