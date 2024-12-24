# | Lab 3 | P3309 | Тупиченко Мила |

## Цель

Освоиться с построением пользовательских типов данных, полиморфизмом, рекурсивными алгоритмами и средствами
тестирования (unit testing, property-based testing), а также разделением интерфейса и особенностей реализации.

## Задание

- Функции:

    - добавление и удаление элементов;
    - фильтрация;
    - отображение (map);
    - свертки (левая и правая);
    - структура должна быть моноидом.


- Структуры данных должны быть неизменяемыми.
- Библиотека должна быть протестирована в рамках unit testing.
- Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства
  моноида).
- Структура должна быть полиморфной.
- Требуется использовать идиоматичный для технологии стиль программирования. Примечание: некоторые языки позволяют
  получить большую часть API через реализацию небольшого интерфейса. Так как лабораторная работа про ФП, а не про
  экосистему языка -- необходимо реализовать их вручную и по возможности -- обеспечить совместимость.
- Обратите внимание:

- API должно быть реализовано для заданного интерфейса и оно не должно "протекать". На уровне тестов -- в первую очередь
  нужно протестировать именно API (dict, set, bag).
- Должна быть эффективная реализация функции сравнения (не наивное приведение к спискам, их сортировка с последующим
  сравнением), реализованная на уровне API, а не внутреннего представления.

### Реализовать Separate Chaining Hashmap Set

## Реализация
### Структура данных
```OCaml
type 'a entry = { hash : int; value : 'a }
type 'a chain = Empty | Node of { entry : 'a entry; next : 'a chain }
type 'a hashSet = { size : int; chains : 'a chain array }
```

### Фильтрация
```OCaml
let filter hashSet predicate =
  let filter_chain chain =
    let rec aux = function
      | Empty -> Empty
      | Node { entry; next } when predicate entry.value ->
          Node { entry; next = aux next }
      | Node { next; _ } -> aux next
    in
    aux chain
  in
  { size = hashSet.size; chains = Array.map filter_chain hashSet.chains }
```

### Отображение
```OCaml
let map f hashSet =
  let rec map_chain acc = function
    | Empty -> acc
    | Node { entry; next } ->
        let new_entry =
          { hash = hash (f entry.value); value = f entry.value }
        in
        map_chain (add acc new_entry.value) next
  in
  Array.fold_left map_chain
    { size = hashSet.size; chains = Array.make hashSet.size Empty }
    hashSet.chains
```

### Свертка
```OCaml
let fold_left f init hashSet =
  let rec fold_chain acc = function
    | Empty -> acc
    | Node { entry; next } -> fold_chain (f acc entry.value) next
  in

  let rec fold_array acc i =
    match i with
    | i when i >= hashSet.size -> acc
    | i -> fold_array (fold_chain acc hashSet.chains.(i)) (i + 1)
  in

  fold_array init 0

let fold_right f init hashSet =
  let rec fold_chain acc = function
    | Empty -> acc
    | Node { entry; next } -> fold_chain (f acc entry.value) next
  in

  let rec fold_array acc i =
    match i with
    | i when i >= hashSet.size -> acc
    | i -> fold_array (fold_chain acc hashSet.chains.(i)) (i + 1)
  in

  fold_array init 0
```

### Функции моноида
```OCaml
let rec mergeChains (chain1 : 'a chain) (chain2 : 'a chain) : 'a chain =
  match (chain1, chain2) with
  | Empty, _ -> chain2
  | _, Empty -> chain1
  | Node { entry = entry1; next = next1 }, Node { entry = entry2; next = next2 }
    ->
      Node
        {
          entry = entry1;
          next = mergeChains next1 (Node { entry = entry2; next = next2 });
        }

let merge (set1 : 'a hashSet) (set2 : 'a hashSet) : 'a hashSet =
  let newChains =
    Array.init set1.size (fun i ->
        let chain1 = set1.chains.(i) in
        let chain2 = set2.chains.(i) in
        mergeChains chain1 chain2)
  in
  { size = set1.size; chains = newChains }

let rec compare_chains chain1 chain2 =
  match (chain1, chain2) with
  | Empty, Empty -> true
  | Empty, _ | _, Empty -> false
  | Node { entry = e1; next = n1 }, Node { entry = e2; next = n2 } ->
      e1.hash = e2.hash && e1.value = e2.value && compare_chains n1 n2

let compare_sets (set1 : 'a hashSet) (set2 : 'a hashSet) : bool =
  if set1.size != set2.size then
    false
  else
    Array.for_all2 compare_chains set1.chains set2.chains
```

