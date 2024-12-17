open QCheck
open Sc_set (* ваш модуль *)

(* Генератор для множества хэш-таблиц *)
let hashSet_gen =
  Gen.(
    list_size (int_range 0 10) (int_range 0 100)
    |> map (fun l ->
           let set = create 10 in
           List.fold_left (fun acc x -> add acc x) set l))

(* Тест на добавление элемента *)
let add_property =
  Test.make ~name:"add property" hashSet_gen (fun set ->
      let new_set = add set 42 in
      setHas new_set 42 = true)

(* Тест на удаление элемента *)
let remove_property =
  Test.make ~name:"remove property" hashSet_gen (fun set ->
      let new_set = remove set 42 in
      setHas new_set 42 = false)

(* Тест на существование элемента в множестве после добавления *)
let add_contains_property =
  Test.make ~name:"add contains property" hashSet_gen (fun set ->
      let new_set = add set 42 in
      setHas new_set 42 = true)

(* Тест на отсутствие элемента в множестве после удаления *)
let remove_not_contains_property =
  Test.make ~name:"remove not contains property" hashSet_gen (fun set ->
      let new_set = remove set 42 in
      setHas new_set 42 = false)

(* Тест на создание пустого множества *)
let create_empty_property =
  Test.make ~name:"create empty property"
    Gen.(return ())
    (fun () ->
      let set = create 10 in
      Array.for_all (fun chain -> chain = Empty) set.chains)

(* Тест на размер множества после добавления и удаления элементов *)
let size_property =
  Test.make ~name:"size property" hashSet_gen (fun set ->
      let size_before =
        Array.fold_left
          (fun acc chain -> match chain with Empty -> acc | Node _ -> acc + 1)
          0 set.chains
      in
      let new_set = add set 42 in
      let size_after =
        Array.fold_left
          (fun acc chain -> match chain with Empty -> acc | Node _ -> acc + 1)
          0 new_set.chains
      in
      size_after = size_before + 1)

(* Основная функция для запуска тестов *)
let () =
  let tests =
    [
      add_property;
      remove_property;
      add_contains_property;
      remove_not_contains_property;
      create_empty_property;
      size_property;
    ]
  in
  QCheck_runner.run_tests ~verbose:true tests
