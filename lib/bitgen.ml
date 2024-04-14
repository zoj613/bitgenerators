(* Copyright (c) 2024, Zolisa Bleki

  SPDX-License-Identifier: BSD-3-Clause *)

(** This library provides implementations of Psuedo-random number generators
    using an interface heavily inspired by numpy's {{:https://numpy.org/doc/stable/reference/random/bit_generators/index.html} [numpy.random]} module.

    The implementation is functional and requires the user to explicitely manage
    state of a bitgenerator. It also provides an mechanism to ensure that each
    generator is initialized using a high quality initial states by passing
    input seed through {!SeedSequence} (see: {{:https://www.pcg-random.org/posts/developing-a-seed_seq-alternative.html} [this blog post]}.

    Below is a highlevel example of one can generate a list of 
    {@ocaml[
        open Bitgen
        open Stdint
        
        let rec generate_10 i t acc = match i >= 10 with
            | true -> List.rev acc, t
            | false -> match PCG64.next_double t with
                | v, t' -> generate_10 (i + 1) t' (v :: acc)

        let rng = SeedSequence.initialize [] |> PCG64.initialize in
        generate_10 0 rng [] |> fst
        (* - : float list =
            [0.227336022467169663; 0.316758339709752867; 0.797365457332734118;
             0.676254670750974562; 0.391109550601909; 0.332813927866384529;
             0.598308753587189823; 0.186734185603713354; 0.672756044014621302;
             0.941802865269937173] *)
    ]}
*)

module SFC64 = Sfc.SFC64
module PCG64 = Pcg.PCG64
module SeedSequence = Seed.SeedSequence
module Xoshiro256 = Xoshiro.Xoshiro256StarStar
module Philox64 = Philox.Philox
