open Stdint

module type S = sig
    type t 
    val next_uint64 :  t -> uint64 * t
    val next_uint32 : t -> uint32 * t
    val next_double : t -> float * t
    val initialize : Seed.SeedSequence.t -> t
end


module SFC64 = Sfc.SFC64
module PCG64 = Pcg.PCG64
module SeedSequence = Seed.SeedSequence
