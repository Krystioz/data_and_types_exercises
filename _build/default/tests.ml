open OUnit2
open Solutions_lists

let tests =
  "test suite for exercises"
  >::: [
         ("2 el" >:: fun _ -> assert_equal 3 (product [ 1; 2 ]));
         ("empty" >:: fun _ -> assert_equal 0 (product []));
         ("more el" >:: fun _ -> assert_equal 131 (product [ 1; 2; 123; 2; 3 ]));
       ]
 
let tests2 =
  "test suite for solutions"
  >::: [
         ("empty" >:: fun _ -> assert_equal false (first_two_equal []));
         ( "3 el false" >:: fun _ ->
           assert_equal false (first_two_equal [ 2; 3; 4 ]) );
         ( "3 el 2 equal" >:: fun _ ->
           assert_equal true (first_two_equal [ 1; 1; 3 ]) );
         ( "4 5 equal" >:: fun _ ->
           assert_equal false (first_two_equal [ 1; 2; 3; 4; 4 ]) );
         ("one el" >:: fun _ -> assert_equal false (first_two_equal [ 1 ]));
       ]

let tests3 =
  "test suite for lists"
  >::: [
         ("empty" >:: fun _ -> assert_equal 0 (fifth []));
         ("4 elements" >:: fun _ -> assert_equal 0 (fifth [ 2; 3; 4; 4 ]));
         ("5 el = 2" >:: fun _ -> assert_equal 2 (fifth [ 1; 1; 34; 7; 2 ]));
         ("6 el" >:: fun _ -> assert_equal 69 (fifth [ 1; 2; 3; 4; 69; 7 ]));
       ]

let _ = run_test_tt_main tests
let _ = run_test_tt_main tests2
let _ = run_test_tt_main tests3
