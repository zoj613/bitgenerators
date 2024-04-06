open OUnit2


let () =
    let suite = "Run All tests" >:::
        Test_seedseq.tests @
        Test_sfc.tests
    in
    run_test_tt_main suite
