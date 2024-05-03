open Stdint
open Bitgen


module type S = sig
    type t 
    val next_uint32 : t -> uint32 * t
    val initialize : SeedSequence.t -> t
end


let mask1, mask2, mask3, mask4 =
    Uint32.(of_int 0x55555555, of_int 0x33333333, of_int 0x0F0F0F0F, of_int 0x00FF00FF)


(* https://graphics.stanford.edu/~seander/bithacks.html
   https://www.pcg-random.org/posts/how-to-test-with-testu01.html *)
let rev_bits v =
    let open Uint32 in
    (* swap odd and even bits *)
    let v = logor (logand (shift_right v 1) mask1) (shift_left (logand v mask1) 1) in
    (* swap consecutive pairs *)
    let v = logor (logand (shift_right v 2) mask2) (shift_left (logand v mask2) 2) in
    (* swap nibbles *)
    let v = logor (logand (shift_right v 4) mask3) (shift_left (logand v mask3) 4) in
    (* swap bytes *)
    let v = logor (logand (shift_right v 8) mask4) (shift_left (logand v mask4) 8) in
    (* swap 2-byte-long pairs *)
    logor (shift_right v 16) (shift_left v 16)


(* TestU01 can only be used for 32bit input and thus causes it to be more sensitive
   to flaws in the most-significant bits than the least significant bits.
   It is important to test general-purpose generators in bit-reversed form,
   to verify their suitability for applications which use the low-order bits.
   Thankfully, the bitgenerator interface provide a next_uint32 function for
   all the 64-bit PRNG's which outputs the the high and then low 32 bits of the
   unsigned random 64 bits. *)
let make_int32 (module M : S) ~rev =
    let t = SeedSequence.initialize [] |> M.initialize |> ref in
    let bits () = match M.next_uint32 !t with
        | u, t' -> t := t'; rev u |> Uint32.to_int32
    in bits


let to_module = function
    | "xoshiro256" -> (module Xoshiro256 : S)
    | "pcg64" -> (module PCG64 : S)
    | "philox4x64" -> (module Philox4x64 : S)
    | "sfc64" -> (module SFC64 : S)
    | "chacha" -> (module ChaCha : S)
    | "lxm" -> (module LXM : S)
    | _ -> failwith "Unknown PRNG"


let testsuite = function
    | "smallcrush" -> TestU01.Bbattery.small_crush
    | "crush" -> TestU01.Bbattery.crush
    | "bigcrush" -> TestU01.Bbattery.big_crush
    | _ -> failwith "Unknown test name"


let is_rev name = function
    | false -> name
    | true -> name ^ "-rev"


let usage = "
Run TestU01's test suite on the supported bitgenerators.
It offers several batteries of tests including 'Small Crush'
(which consists of 10 tests), 'Crush' (96 tests), and 'Big Crush' (106 tests).

crush [-name] [-pvalue] [-rev] [-verbose] <bitgen>

E.g `crush -name bigcrush -rev pcg64` (which runs Big Crush on the reversed bits of PCG64 generator).
"


let () =
    let pvalue = ref 0.001
    and test = ref "smallcrush"
    and prng = ref ""
    and verbose = ref false
    and rev = ref false in
    let spec = [
        ("-pvalue", Arg.Set_float pvalue, "Threshold for suspect p-values. Defaults to 0.001");
        ("-name", Arg.Set_string test, "Name of the test to run. Should be one of {smallcrush, crush, bigcrush}. Defaults to 'smallcrush'");
        ("-rev", Arg.Set rev, "Run tests on reversed bits of the input.");
        ("-verbose", Arg.Set verbose, "In addition to the summary report, write the result of each test to stdout.");
    ] in
    Arg.parse spec (fun x -> prng := x) usage;
    TestU01.Swrite.set_basic !verbose;
    Probdist.Gofw.set_suspectp !pvalue;
    (to_module !prng |> make_int32 ~rev:(if !rev then rev_bits else Fun.id))
    |> TestU01.Unif01.create_extern_gen_int32 (is_rev !prng !rev) 
    |> testsuite !test
