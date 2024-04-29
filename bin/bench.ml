open Bitgen

module type S = sig
    type t 
    val next_uint64 : t -> Stdint.uint64 * t
    val initialize : SeedSequence.t -> t
end


let make_bits (module M : S) seed =
    let t = ref (SeedSequence.initialize [Stdint.Uint128.of_int seed] |> M.initialize)
    in (fun () -> t := (M.next_uint64 !t |> snd))


let pairs = [
    "PCG64", (module PCG64: S);
    "SFC64", (module SFC64: S);
    "Xoshiro256", (module Xoshiro256: S);
    "Philox4x64", (module Philox4x64: S);
    "ChaCha", (module ChaCha: S);
]


let make_fn seed (name, m) =
    Core_bench.Bench.Test.create ~name:(name ^ ".next_uint64") (make_bits m seed)


let () =
    let seed = 123456789 in
    Stdlib.Random.init seed;
    [Core_bench.Bench.Test.create ~name:"Stdlib.Random.bits64" Stdlib.Random.bits64] @
    List.map (make_fn seed) pairs
    |> Core_bench.Bench.make_command
    |> Command_unix.run
