open QCheck
open Sc_set

let from_list lst = List.fold_left add (create ()) lst

 (* Test: Associativity *)
 let test_associativity =
  Test.make ~name:"Associativity of merging two sets"
    (triple (list int) (list int) (list int))
    (fun (lst1, lst2, lst3) ->
      let set1 = from_list lst1 in
      let set2 = from_list lst2 in
      let set3 = from_list lst3 in
      merge (merge set1 set2) set3 = merge set1 (merge set2 set3))

(* Test: Neutral Element *)
let test_neutral_element =
  Test.make ~name:"Neutral element for merging" (list int) (fun lst ->
      let set = from_list lst in
      let empty_set = create () in
      merge set empty_set = set)

(* Test: Insertion *)
let test_insertion =
  Test.make ~name:"Insertion adds an element"
    (pair (list int) int)
    (fun (lst, x) ->
      let set = from_list lst in
      let set_with_x = add set x in
      setHas set_with_x x)

(* Test: Removal *)
let test_removal =
  Test.make ~name:"Removal removes an element"
    (pair (list int) int)
    (fun (lst, x) ->
      let set = from_list lst in
      let set_without_x = remove set x in
      not (setHas set_without_x x))

(* Test: Merge *)
let test_merge =
  Test.make ~name:"Merge contains elements from both sets"
    (pair (list int) (list int))
    (fun (lst1, lst2) ->
      let set1 = from_list lst1 in
      let set2 = from_list lst2 in
      let merged = merge set1 set2 in
      (List.for_all (setHas merged) lst1) || (List.for_all (setHas merged) lst2))

(* Test: Filter *)
let test_filter =
  Test.make ~name:"Filter retains elements satisfying predicate" (list int)
    (fun lst ->
      let set = from_list lst in
      let filtered_set = filter set (fun x -> x mod 2 = 0) in
      List.for_all
        (fun x ->
          (x mod 2 = 0 && setHas filtered_set x)
          || ((not (x mod 2 = 0)) && not (setHas filtered_set x)))
        lst)

(* Test: Map *)
let test_map =
  Test.make ~name:"Map applies a function to all elements" (list int)
    (fun lst ->
      let set = from_list lst in
      let f x = x * 2 in
      let mapped_set = map f set in
      List.for_all (fun x -> setHas mapped_set (f x)) lst)

(* Test: Fold_left *)
let test_fold_left =
  Test.make ~name:"Fold_left computes correctly" (list int) (fun lst ->
      let set = from_list lst in
      let sum = fold_left ( + ) 0 set in
      sum = List.fold_left ( + ) 0 lst)

(* Test: Fold_right *)
let test_fold_right =
  Test.make ~name:"Fold_right computes correctly" (list int) (fun lst ->
      let set = from_list lst in
      let sum = fold_right ( + ) 0 set in
      sum = List.fold_right ( + ) lst 0)

(* Running the tests *)
let () =
  QCheck_runner.run_tests_main
    [
      test_associativity;
      test_neutral_element;
      test_insertion;
      test_removal;
      test_merge;
      test_filter;
      test_map;
      test_fold_left;
      test_fold_right;
    ]
