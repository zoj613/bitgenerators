open Stdint


module PCG64 : sig
    type t
    val next_uint64 :  t -> uint64 * t
    val next_uint32 : t -> uint32 * t
    val next_double : t -> float * t
    val initialize : Seed.SeedSequence.t -> t
    val advance : int128 -> t -> t
    val next_bounded_uint64 : uint64 -> t -> uint64 * t
    val set_seed : uint64 * uint64 * uint64 * uint64 -> t -> t
end = struct
    type t = {s : setseq; has_uint32 : bool; uinteger : uint32}
    and setseq = {state : uint128; increment : uint128}

    let multiplier = Uint128.of_string "0x2360ed051fc65da44385df649fccf645"

    (* Uses the XSL-RR output function *)
    let output state =
        let v = Uint128.(shift_right state 64 |> logxor state |> Uint64.of_uint128) in
        let r = Uint128.(shift_right state 122 |> to_int) in
        Uint64.((-r) land 63 |> shift_left v |> logor (shift_right v r))


    let next {state; increment} =
        let setseq' = {state = Uint128.(state * multiplier + increment); increment} in
        output setseq'.state, setseq'


    let next_setseq initstate initseq =
        let open Uint128 in
        let increment = shift_left initseq 1 |> logor one in
        let state = zero * multiplier + increment in
        {state = (state + initstate) * multiplier + increment; increment}


    let next_uint64 t =
        let uint, s' = next t.s in
        uint, {t with s = s'}
    

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


    let advance_state_lcg {state; increment} delta mult =
        let open Uint128 in
        let rec loop d am ap cm cp =
            match d = zero, logand d one = one with
            | true, _ -> am, ap
            | false, true -> loop (shift_right d 1) (am * cm) (ap * cm + cp) (cm * cm) (cp * (cm + one))
            | false, false -> loop (shift_right d 1) am ap (cm * cm) (cp * (cm + one))
        in
        let am, ap = loop delta one zero mult increment in
        am * state + ap


    let advance delta t =
        {s = {t.s with state = advance_state_lcg t.s (Uint128.of_int128 delta) multiplier};
         has_uint32 = false;
         uinteger = Uint32.zero}


    let set_seed (shi, slo, ihi, ilo) t =
        let s = Uint128.(logor (shift_left (of_uint64 shi) 64) (Uint64.to_uint128 slo)) in
        let i = Uint128.(logor (shift_left (of_uint64 ihi) 64) (Uint64.to_uint128 ilo)) in
        {t with s = next_setseq s i}

    (* https://www.pcg-random.org/posts/bounded-rands.html *)
    let next_bounded_uint64 bound t =
        let rec loop threshold = function
            | r, s when r >= threshold -> s, r
            | _, s -> loop threshold (next s)
        in
        let setseq', r' = loop (Uint64.rem (Uint64.neg bound) bound) (next t.s) in
        Uint64.rem r' bound, {t with s = setseq'} 


    let initialize seed =
        let t = {s = {state = Uint128.zero; increment = Uint128.zero};
                 has_uint32 = false;
                 uinteger = Uint32.zero} in
        let istate = Seed.SeedSequence.generate_64bit_state 4 seed in
        set_seed (istate.(0), istate.(1), istate.(2), istate.(3)) t
end
