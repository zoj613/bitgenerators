open Stdint

module Philox : sig
    type t 
    val next_uint64 :  t -> uint64 * t
    val next_uint32 : t -> uint32  * t
    val next_double : t -> float * t
    val initialize : Seed.SeedSequence.t -> t
    val jump : t -> t

end = struct

    type t = {
        ctr : counter;
        key: key;
        buffer_pos : int;
        buffer : uint64 array;
        has_uint32 : bool;
        uinteger : uint32}
    and counter = uint64 * uint64 * uint64 * uint64
    and key = uint64 * uint64


    let bumpkey (x, y) =
        Uint64.(x + Uint64.of_string "0x9E3779B97F4A7C15",
                y + Uint64.of_string "0xBB67AE8584CAA73B")


    let mulhilo64 a b =
        let p = Uint128.(of_uint64 a * of_uint64 b) in
        Uint128.(shift_right p 64 |> to_uint64, to_uint64 p)


    let round (c0, c1, c2, c3) (k0, k1) =
        let hi0, lo0 = mulhilo64 (Uint64.of_string "0xD2E7470EE14C6C93") c0 in
        let hi1, lo1 = mulhilo64 (Uint64.of_string "0xCA5A826395121157") c2 in
        Uint64.(logxor hi1 c1 |> logxor k0, lo1,
                logxor hi0 c3 |> logxor k1, lo0)


    let ten_rounds ctr key =
        let rec loop r = function
            | (c, k) when r >= 9 -> c, k
            | (c, k) -> loop (r + 1) (round_and_bump c k)
        and round_and_bump ctr key = round ctr key, bumpkey key
        in
        let ctr', key' = loop 0 (ctr, key) in
        round ctr' key'


    let next (c0, c1, c2, c3) =
        let open Uint64 in
        let c0' = c0 + one in
        if c0' = zero then
            let c1' = c1 + one in
            if c1' = zero then
                let c2' = c2 + one in
                if c2' = zero then
                    (c0', c1', c2', c3 + one)
                else
                    (c0', c1', c2', c3)
            else
                (c0', c1', c2, c3)
        else 
            (c0', c1, c2, c3)


    let to_array (c0, c1, c2, c3) = [| c0; c1; c2; c3 |]


    let next_uint64 t =
        if t.buffer_pos < 4 then
            t.buffer.(t.buffer_pos), {t with buffer_pos = t.buffer_pos + 1}
        else
            let ctr' = next t.ctr in
            let buf = ten_rounds ctr' t.key |> to_array in
            buf.(0), {t with ctr = ctr'; buffer = buf; buffer_pos = 1}
     

    let next_uint32 t =
        match t.has_uint32 with
        | true ->
            t.uinteger, {t with has_uint32 = false}
        | false ->
            let uint, t' = next_uint64 t in
            Uint64.(of_int 0xffffffff |> logand uint |> to_uint32),
            {t' with
             has_uint32 = true;
             uinteger = Uint64.(shift_right uint 32 |> to_uint32)}


    let next_double t =
        let uint, t' = next_uint64 t in
        Uint64.shift_right uint 11
        |> Uint64.to_string
        |> Float.of_string
        |> Float.mul (1.0 /. 9007199254740992.0), t'


    let jump t =
        let c0, c1, c2, c3 = t.ctr in
        let c2' = Uint64.(c2 + one) in
        match Uint64.(c2' = zero) with
        | true -> {t with ctr = (c0, c1, c2', Uint64.(c2' + one))}
        | false -> {t with ctr = (c0, c1, c2', c3)}


    let initialize seed =
        let istate = Seed.SeedSequence.generate_64bit_state 2 seed in
        {ctr = Uint64.(zero, zero, zero, zero);
         buffer = Array.make 4 Uint64.zero;
         key = (istate.(0), istate.(1));
         uinteger = Uint32.zero;
         has_uint32 = false;
         buffer_pos = 4}
end
