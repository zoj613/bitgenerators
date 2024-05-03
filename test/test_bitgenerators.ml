open OUnit2


let () =
    let suite = "Run All tests" >:::
        Test_seedseq.tests @
        Test_sfc.tests @
        Test_pcg.tests @
        Test_philox.tests @
        Test_xoshiro.tests @
        Test_chacha.tests @
        Test_lxm.tests

    in
    run_test_tt_main suite
