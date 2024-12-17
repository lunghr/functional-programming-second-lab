open QCheck
open Sc_set (* ваш модуль *)

let int_gen = Gen.int 0 100
let add a b = a + b

(* Свойство для проверки коммутативности *)
let commutative_property =
  Test.make ~name:"commutative property" int_gen (fun (a, b) ->
      add a b = add b a)
(* проверяем, что a + b = b + a *)

(* Свойство для проверки ассоциативности *)
let associative_property =
  Test.make ~name:"associative property" (Gen.pair int_gen int_gen)
    (fun (a, (b, c)) -> add (add a b) c = add a (add b c))
(* проверяем, что (a + b) + c = a + (b + c) *)

(* Запуск тестов *)
let () =
  let tests = [ commutative_property; associative_property ] in
  QCheck_runner.run_tests ~verbose:true tests
