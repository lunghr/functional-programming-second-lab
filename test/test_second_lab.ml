open Sc_set
open OUnit2


let len = 10
let set = create len
let set = add set 1
let set = add set 2
let set = add set 3
let set = add set 4
let set = add set 5

let string_set = create len
let string_set = add string_set "a"
let string_set = add string_set "b"
let string_set = add string_set "c"
let string_set = add string_set "d"
let string_set = add string_set "e"

 let even x = x mod 2 = 0

let test_add _ =
    assert_equal true (setHas set 1);
    assert_equal true (setHas set 2);
    assert_equal true (setHas set 3);
    assert_equal true (setHas set 4);
    assert_equal true (setHas set 5);
    assert_equal true (setHas string_set "a");
    assert_equal true (setHas string_set "b");
    assert_equal true (setHas string_set "c");
    assert_equal true (setHas string_set "d");
    assert_equal true (setHas string_set "e")


 let test_remove _ =
    let set = remove set 1 in
    let set = remove set 2 in
    let set = remove set 3 in
    let set = remove set 4 in
    let set = remove set 5 in
    assert_equal false (setHas set 1);
    assert_equal false (setHas set 2);
    assert_equal false (setHas set 3);
    assert_equal false (setHas set 4);
    assert_equal false (setHas set 5);
    let string_set = remove string_set "a" in
    let string_set = remove string_set "b" in
    let string_set = remove string_set "c" in
    let string_set = remove string_set "d" in
    let string_set = remove string_set "e" in
    assert_equal false (setHas string_set "a");
    assert_equal false (setHas string_set "b");
    assert_equal false (setHas string_set "c");
    assert_equal false (setHas string_set "d");
    assert_equal false (setHas string_set "e")


 let test_filter _ =
    let set = filter set even in
    assert_equal false (setHas set 1);
    assert_equal true (setHas set 2);
    assert_equal false (setHas set 3);
    assert_equal true (setHas set 4);
    assert_equal false (setHas set 5);
    let string_set = filter string_set (fun x -> x = "a" || x = "c" || x = "e") in
    assert_equal true (setHas string_set "a");
    assert_equal false (setHas string_set "b");
    assert_equal true (setHas string_set "c");
    assert_equal false (setHas string_set "d");
    assert_equal true (setHas string_set "e")

let test_map _ =
    let set = map (fun x -> x * 2) set  in
    assert_equal false (setHas set 1);
    assert_equal true (setHas set 2);
    assert_equal false (setHas set 3);
    assert_equal true (setHas set 4);
    assert_equal false (setHas set 5);
    assert_equal true (setHas set 6);
    assert_equal true (setHas set 8);
    assert_equal true (setHas set 10);
    let string_set = map (fun x -> x ^ x) string_set in
    assert_equal true (setHas string_set "aa");
    assert_equal true (setHas string_set "bb");
    assert_equal true (setHas string_set "cc");
    assert_equal true (setHas string_set "dd");
    assert_equal true (setHas string_set "ee")

let test_fold_left _ =
    let sum = fold_left (fun acc x -> acc + x) 0 set in
    assert_equal 15 sum;
    let concat = fold_left (fun acc x -> acc ^ x) "" string_set
    in
    assert_equal "eabcd" concat


 let test_fold_right _ =
    let sum = fold_right (fun x acc -> acc + x) 0 set in
    assert_equal 15 sum;
    let concat = fold_right (fun x acc -> acc ^ x) "" string_set in
    assert_equal "dcbae" concat

let test_merge _ =
    let set1 = create len in
    let set1 = add set1 1 in
    let set1 = add set1 2 in
    let set1 = add set1 3 in
    let set1 = add set1 4 in
    let set1 = add set1 5 in
    let set2 = create len in
    let set2 = add set2 6 in
    let set2 = add set2 7 in
    let set2 = add set2 8 in
    let set2 = add set2 9 in
    let set2 = add set2 10 in
    let set = merge set1 set2 in
    Printf.printf "%s\n" (string_of_hashSet set);
    assert_equal true (setHas set 1);
    assert_equal true (setHas set 2);
    assert_equal true (setHas set 3);
    assert_equal true (setHas set 4);
    assert_equal true (setHas set 5);
    assert_equal true (setHas set 6);
    assert_equal true (setHas set 7);
    assert_equal true (setHas set 8);
    assert_equal true (setHas set 9);
    assert_equal true (setHas set 10)

let test_compare _ =
    let set1 = create len in
    let set1 = add set1 1 in
    let set1 = add set1 2 in
    let set1 = add set1 3 in
    let set1 = add set1 4 in
    let set1 = add set1 5 in
    let set2 = create len in
    let set2 = add set2 6 in
    let set2 = add set2 7 in
    let set2 = add set2 8 in
    let set2 = add set2 9 in
    let set2 = add set2 10 in
    assert_equal false (compare_sets set1 set2);
    assert_equal true (compare_sets set1 set)

let test_associative _ =
    let set1 = create 10 in
    let set1 = add set1 "a" in
    let set1 = add set1 "b" in  (* Переприсваиваем результат *)
    let set2 = create 10 in
    let set2 = add set2 "c" in
    let set2 = add set2 "d" in
    let set3 = create 10 in
    let set3 = add set3 "e" in
    let test_assoc = merge (merge set1 set2) set3 = merge set1 (merge set2 set3) in
    assert_equal true test_assoc

let test_commutative _ =
    let set1 = create 10 in
    let set1 = add set1 "a" in
    let set1 = add set1 "b" in  (* Переприсваиваем результат *)
    let set2 = create 10 in
    let set2 = add set2 "c" in
    let set2 = add set2 "d" in
    let test_comm = merge set1 set2 = merge set2 set1 in
    assert_equal true test_comm



    (* Тест на идемпотентность *)
let suite = "test_second_lab" >::: [
    "test_add" >:: test_add;
    "test_remove" >:: test_remove;
    "test_filter" >:: test_filter;
    "test_map" >:: test_map;
    "test_fold_left" >:: test_fold_left;
    "test_fold_right" >:: test_fold_right;
    "test_merge" >:: test_merge;
    "test_compare" >:: test_compare;
    "test_associative" >:: test_associative;
    "test_commutative" >:: test_commutative
]

let () =
    run_test_tt_main suite