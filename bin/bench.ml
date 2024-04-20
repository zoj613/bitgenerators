open Bitgen

module type S = sig
    type t 
    val next_uint64 : t -> Stdint.uint64 * t
    val initialize : SeedSequence.t -> t
end


let make_bits (module M : S) =
    let ss = SeedSequence.initialize [Stdint.Uint128.of_int 123456789] in
    let t = ref (M.initialize ss) in
    let bits () = match M.next_uint64 !t with
        | (u, t') -> t := t'; u
    in
    bits


let pairs = [
    "PCG64", (module PCG64: S);
    "SFC64", (module SFC64: S);
    "Xoshiro256", (module Xoshiro256: S);
    "Philox64", (module Philox64: S);
    "ChaCha", (module ChaCha: S);
]


let make_fn (name, m) =
    Core_bench.Bench.Test.create ~name:(name ^ ".next_uint64") (make_bits m)


let () =
    Stdlib.Random.init 123456789;
    [Core_bench.Bench.Test.create ~name:"Stdlib.Random.bits64" (fun () -> Stdlib.Random.int64 Int64.max_int)] @
    List.map make_fn pairs
    |> Core_bench.Bench.make_command
    |> Command_unix.run
