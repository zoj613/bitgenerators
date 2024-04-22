open OUnit2
open Stdint


let inputs = [
    [3735928559; 195939070; 229505742; 305419896];
    [3668361503; 4165561550; 1661411377; 3634257570];
    [164546577; 4166754639; 1765190214; 1303880213];
    [446610472; 3941463886; 522937693; 1882353782];
    [1864922766; 1719732118; 3882010307; 1776744564];
    [4141682960; 3310988675; 553637289; 902896340];
    [1134851934; 2352871630; 3699409824; 2648159817];
    [1240956131; 3107113773; 1283198141; 1924506131];
    [2669565031; 579818610; 3042504477; 2774880435];
    [2766103236; 2883057919; 4029656435; 862374500];
]

let outputs32 = [
    [3914649087; 576849849; 3593928901; 2229911004];
    [2240804226; 3691353228; 1365957195; 2654016646];
    [3562296087; 3191708229; 1147942216; 3726991905];
    [1403443605; 3591372999; 1291086759; 441919183];
    [1086200464; 2191331643; 560336446; 3658716651];
    [3249937430; 2346751812; 847844327; 2996632307];
    [2584285912; 4034195531; 3523502488; 169742686];
    [959045797; 3875435559; 1886309314; 359682705];
    [3978441347; 432478529; 3223635119; 138903045];
    [296367413; 4262059219; 13109864; 3283683422];
]

let outputs64 = [
    ["2477551240072187391"; "9577394838764454085"];
    ["15854241394484835714"; "11398914698975566411"];
    ["13708282465491374871"; "16007308345579681096"];
    ["15424829579845884309"; "1898028439751125927"];
    ["9411697742461147792"; "15714068361935982142"];
    ["10079222287618677782"; "12870437757549876199"];
    ["17326737873898640088"; "729039288628699544"];
    ["16644868984619524261"; "1544825456798124994"];
    ["1857481142255628931"; "596584038813451439"];
    ["18305404959516669237"; "14103312907920476776"];
]

(* test_reference_data*bit functions test the output against that of the
   reference implementation in C++ (https://gist.github.com/imneme/540829265469e673d045)
   as well as that of numpy's numpy.random.SeedSequence *)
let test_against_reference _ =
    let open Bitgen.SeedSequence in
    let gen_state32 n x =
        List.map Uint128.of_int x
        |> initialize
        |> generate_32bit_state n
        |> Array.map Uint32.to_int
        |> Array.to_list
    and gen_state64 n x =
        List.map Uint128.of_int x
        |> initialize
        |> generate_64bit_state n
        |> Array.map Uint64.to_string
        |> Array.to_list
    in
    assert_equal
        (List.concat outputs64)
        (List.map (gen_state64 2) inputs |> List.concat)
        ~printer:(List.fold_left (fun acc x -> acc ^ x ^ ", ") "");
    assert_equal
        (List.concat outputs32)
        (List.map (gen_state32 4) inputs |> List.concat)
        ~printer:(List.fold_left (fun acc x -> acc ^ string_of_int x ^ ", ") "")


let test_zero_padding _ =
    (* Ensure that large integers are inserted in little-endian fashion to avoid  trailing 0s.*)
    let open Bitgen.SeedSequence in
    assert_equal
        (generate_32bit_state 4 (initialize [Uint128.of_int 42]))
        (generate_32bit_state 4 (initialize [42 lsl 32 |> Uint128.of_int]))
        ~cmp:(fun x y -> x <> y);
    (* Ensure that the implicit 0s don't conflict with spawn keys. *)
    assert_equal
        (generate_32bit_state 4 (initialize [Uint128.of_int 42]))
        (generate_32bit_state 4 (initialize ~spawn_key:[Uint128.zero] [Uint128.of_int 42]))
        ~cmp:(fun x y -> x <> y)


let test_spawn_children _ =
    let open Bitgen.SeedSequence in
    let children, t = initialize [] |> spawn 10 in
    assert_bool "children_spawned not equal to 10" (List.length children = 10);
    assert_bool "children_spawned not equal to 10" (children_spawned t = 10);
    assert_bool "child entropy not equal parent's" (entropy t = entropy (List.hd children))


let tests = [
    "Test SeedSequence output values against reference data" >:: test_against_reference;
    "test correct numnber of spawn children" >:: test_spawn_children;
    "test zero padding output" >:: test_zero_padding
]
