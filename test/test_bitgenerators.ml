open OUnit2


let suite = "Run All tests" >:::
    Test_seedseq.tests


let () =
    run_test_tt_main suite
