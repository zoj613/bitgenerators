open Stdint


module EFIIX64 : sig
  (** EFIIX64x48 is a 64-bit PRNG that uses a set of tables to generate random values.
      This produces a fast PRNG with statistical quality similar to cryptographic
      generators but faster.

      The EFIIX64x48 state vector consists of a 16-element array of 64-bit
      unsigned integers and a 32-element array of 64-bit unsigned integers.
      In addition, 3 constant values and a counter are used in the update.

      The initial state of the bitgenerator is provided using {!SeedSequence.t}. *)

  include Common.BITGEN
end = struct
  type t = {s : state; ustore : uint32 option}
  and state = {ind_table : uint64 array; iter_table : uint64 array;
               i : uint64; a : uint64; b : uint64; c : uint64}
  type arbee = {a : uint64; b : uint64; c : uint64; d : uint64; i : uint64}


  let rotate x k = let y = 64 - k in Uint64.(logor (shift_left x k) (shift_right x y))


  let iter_size, ind_size = Uint64.(of_int 32, of_int 16)

  let next (s : state) =
    let open Uint64 in
    let i = rem s.i iter_size |> to_int in
    let j = rem s.c ind_size |> to_int in
    (* copying and assigning is much faster than using Array.mapi here *)
    let ind_table, iter_table = Array.(copy s.ind_table, copy s.iter_table) in
    ind_table.(j) <- s.iter_table.(i) + s.a; iter_table.(i) <- s.ind_table.(j);

    let b = s.c + s.ind_table.(j) in
    logxor b s.iter_table.(i),
    {ind_table; iter_table; b; a = s.b + s.i; c = logxor s.a s.b + rotate s.c 25; i = s.i + one}


    let next_uint64 t = let u, s = next t.s in u, {t with s}


    let next_uint32 t = match Common.next_uint32 ~next:next t.s t.ustore with
      | u, s, ustore -> u, {s; ustore}


    let next_double t = Common.next_double ~nextu64:next_uint64 t


    let next_bounded_uint64 bound t = Common.next_bounded_uint64 bound ~nextu64:next_uint64 t


    let arbee_next (s : arbee) =
      let open Uint64 in
      let e = s.a + rotate s.b 45 in
      let a = logxor s.b (rotate s.c 13) in
      let d = e + a in
      {a; b = s.c + rotate s.d 37; c = e + s.d + s.i; d; i = s.i + one}, d


    let rec mix (s : arbee) = function
      | 0 -> s
      | i -> mix (arbee_next s |> fst) (i - 1)


    let rec discard (s : state) = function
      | 0 -> s
      | i -> discard (next s |> snd) (i - 1)


    let arbee_seed seed =
      mix {a = seed.(0); b = seed.(1); c = seed.(2); d = seed.(3); i = Uint64.one} 12


    let set_seed seed =
      let seeder, ind_table = Array.fold_left_map
        (fun acc _ -> arbee_next acc) (arbee_seed seed) (Array.make 16 0) in
      let seeder, i = arbee_next seeder in
      let iter_table = (Array.make 32 Uint64.zero) in
      let seeder, iter_values = Array.fold_left_map
        (fun acc _ -> arbee_next acc) seeder iter_table  in
      Array.iteri
        (fun idx v -> iter_table.(Uint64.(rem (of_int idx + i) iter_size |> to_int)) <- v) iter_values;
      let seeder, a = arbee_next seeder in
      let seeder, b = arbee_next seeder in
      let seeder, c = arbee_next seeder in
      let s = discard {ind_table; iter_table; i; a; b; c} 64 in

      let seeder = arbee_next seeder |> fst in
      let seeder, s1 = arbee_next seeder in
      let seeder, s2 = arbee_next seeder in
      let seeder = arbee_seed Uint64.[|s1 + seed.(0) |> logxor s.a; s2 + seed.(1) |> logxor s.b;
                                       (arbee_next seeder |> snd) + seed.(2) |> logxor s.c; lognot seed.(3)|] in
      let ind_table = Array.fold_left_map
        (fun acc y -> let acc', x = arbee_next acc in acc', Uint64.logxor x y) seeder s.ind_table |> snd in
      discard {s with ind_table} 48


    let initialize seed =
      {s = Seed.SeedSequence.generate_64bit_state 4 seed |> set_seed; ustore = None}
end
