open Stdint


module LXM : sig
    (** The LXM generator combines two simple generators with an optional
        additional hashing of the output. The first generator is a LCG of with
        and update {m s = a s + b \mod 2^{64}} where {m a} is {m 2862933555777941757}
        and {m b} is settable. The default value of {m b} is {m 3037000493}.
        The second is the standard 64-bit xorshift generator.

        The output of these two is combined using addition. This sum is hashed
        using the Murmur3 hash function using the parameters suggested by David Stafford.
        Is pseudo-code, each value is computed as [Mix(LCG + Xorshift)]. 

        The LXM state vector consists of a 4-element array of 64-bit
        unsigned integers that constraint the state of the Xorshift generator
        and an addition 64-bit unsigned integer that holds the state of the
        LCG. These initial states are set using {!SeedSequence}

        LXM can be used in parallel applications by calling {!LXM.jump} which
        provides a new instance with a state that has been updated as-if {m 2^{128}}
        random numbers have been generated. This allows the original sequence to be
        split so that distinct segments can be used in each worker process. *)

    include Common.BITGEN

    val initialize_full : uint64 -> Seed.SeedSequence.t  -> t
    (** [initialize_full b seedseq] initialiazes the state of the LXM generator
        where [b] is the settable additive constant of the underlying LCG generator. *)

    val jump : t -> t
    (** [jump t] is equivalent to {m 2^{128}} calls to {!LXM.next_uint64}. *)
end = struct
  type t = {state : uint64 array; lcg_state : uint64; b : uint64; ustore : uint32 option}


  let rotl x k = let y = 64 - k in Uint64.(logor (shift_left x k) (shift_right x y))


  (* https://nuclear.llnl.gov/CNP/rng/rngman/node4.html *)
  let multiplier = Uint64.of_int64 2862933555777941757L
  let p0, p1 = Uint64.(of_string "0xbf58476d1ce4e5b9", of_string "0x94d049bb133111eb")


  (* https://zimbry.blogspot.com/2011/09/better-bit-mixing-improving-on.html
     https://prng.di.unimi.it/splitmix64.c *)
  let murmur_hash3 key =
    Uint64.(logxor key (shift_right key 30) * p0)
    |> (fun k -> Uint64.(logxor k (shift_right k 27) * p1))
    |> (fun k -> Uint64.(logxor k (shift_right k 31)))


  (* https://prng.di.unimi.it/xoshiro256plus.c *)
  let xorshift s = 
    let open Uint64 in
    let x2 = logxor s.(2) s.(0) and x3 = logxor s.(3) s.(1) in
    [|logxor s.(0) x3; logxor s.(1) x2; shift_left s.(1) 17 |> logxor x2; rotl x3 45|]


  let next_uint64 t =
    murmur_hash3 Uint64.(t.state.(0) + t.lcg_state),
    {t with lcg_state = Uint64.(t.lcg_state * multiplier + t.b); state = xorshift t.state}


  let next_uint32 t = match Common.next_uint32 ~next:next_uint64 t t.ustore with
    | u, s, ustore -> u, {s with ustore}


  let next_double t = Common.next_double ~nextu64:next_uint64 t


  let next_bounded_uint64 bound t = Common.next_bounded_uint64 bound ~nextu64:next_uint64 t


  let zeros = Uint64.[|zero; zero; zero; zero|]
  let jump = Uint64.([|of_int 0x180ec6d33cfd0aba; of_string "0xd5a61266f0c9392c";
                       of_string "0xa9582618e03fc9aa"; of_int 0x39abdc4529b1661c|])
  let jump t = 
    let rec loop b j (acc0, acc1) =
        match b >= 64, Uint64.(logand j (shift_left one b) > zero) with
        | true, _ -> acc0, acc1 
        | false, true -> loop (b + 1) j (Array.map2 Uint64.logxor acc0 acc1, xorshift acc1)
        | false, false -> loop (b + 1) j (acc0, xorshift acc1)
    in {t with state = Array.fold_right (loop 0) jump (zeros, t.state) |> fst; ustore = None}


  let initialize_full b seed =
    (* Protect against negligible prob of all 0 in Xorshift. *)
    let rec loop state = function
      | b when b <> Uint64.zero -> state
      | b ->
          let state' = Seed.SeedSequence.generate_64bit_state 5 seed in
          loop state' (Array.sub state' 0 4 |> Array.fold_left Uint64.logor b)
    in
    let s = loop (Seed.SeedSequence.generate_64bit_state 5 seed) Uint64.zero in
    {state = Array.sub s 0 4; lcg_state = s.(4); b = Uint64.(logor b one); ustore = None}


  let initialize seed = initialize_full (Uint64.of_int64 3037000493L) seed
end
