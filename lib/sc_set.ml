(**)
type 'a entry = {hash: int; value: 'a}
type 'a chain =
    | Empty
    | Node of {
        entry: 'a entry;
        next: 'a chain
    }
type 'a hashSet = {
    size: int;
    chains: 'a chain array
}

(* Функция для отображения цепочки *)
let rec string_of_chain = function
  | Empty -> ""
  | Node node ->
      let entry_str = Printf.sprintf "{hash = %d; value = %d}" node.entry.hash node.entry.value in
      entry_str ^ " -> " ^ (string_of_chain node.next)

(* Функция для отображения hashSet *)
let string_of_hashSet hashSet =
  Array.fold_left (fun acc chain ->
    acc ^ (string_of_chain chain) ^ "\n"
  ) "" hashSet.chains

let create size = {
    size = size;
    chains = Array.make size Empty
}

let hash value=
    Hashtbl.hash value

let add hashSet value =
    let key = hash value in
    let index = key mod hashSet.size in

    let rec update_chain chain =
        match chain with
        | Empty -> Node {entry = {hash = key; value = value}; next = Empty}
        | Node node ->
            match node.entry.hash with
            |k when k = key -> Node {entry = {hash = key; value = value}; next = node.next}
            |_ -> Node {entry = node.entry; next = update_chain node.next}
    in

    let new_chains = Array.mapi
        (fun i c -> if i = index then update_chain c else c)
        hashSet.chains
    in
    {hashSet with chains = new_chains}


(* let remove hashSet value = *)
(*  let key = hash value in *)
(*  let index = key mod hashSet.size in *)
(*  let rec loop = function *)
(*    | Empty -> Empty *)
(*    | Node node when node.entry.hash = key -> node.next *)
(*    | Node node -> Node { entry = node.entry; next = loop node.next } *)
(*  in *)
(*  let new_chains = *)
(*    Array.mapi *)
(*      (fun i chain -> if i = index then loop chain else chain) *)
(*      hashSet.chains *)
(*  in *)
(*  { size = hashSet.size; chains = new_chains } *)
let remove hashSet value =
  let key = hash value in
  let index = key mod hashSet.size in
  let rec remove_from_chain = function
    | Empty -> Empty
    | Node { entry; next } when entry.hash = key -> next
    | Node { entry; next } -> Node { entry; next = remove_from_chain next }
  in
  {
    size = hashSet.size;
    chains =
      Array.mapi
        (fun i chain -> if i = index then remove_from_chain chain else chain)
        hashSet.chains;
  }

let setHas hashSet value =
    let key = hash value in
    let index = key mod hashSet.size in
    let rec loop = function
        | Empty -> false
        | Node node when node.entry.hash = key -> true
        | Node node -> loop node.next
    in
    loop hashSet.chains.(index)


(* let filter hashSet predicate = *)
(*  let rec filter_chain = function *)
(*    | Empty -> Empty *)
(*    | Node node when predicate node.entry.value -> *)
(*        Node { entry = node.entry; next = filter_chain node.next } *)
(*    | Node node -> filter_chain node.next *)
(*  in *)
(*  *)
(*  let new_chains = *)
(*    Array.mapi (fun _ chain -> filter_chain chain) hashSet.chains *)
(*  in *)
(*  *)
(*  { size = hashSet.size; chains = new_chains } *)

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
  {
    size = hashSet.size;
    chains = Array.map filter_chain hashSet.chains;
  }


(*  *)
(* let map f hashSet = *)
(*    let rec collect_chain acc = function *)
(*        | Empty -> acc *)
(*        | Node node -> collect_chain (Node { entry = node.entry; next = acc }) node.next *)
(*      in *)
(*      let rec collect_all i acc = *)
(*        match i with *)
(*        | i when i >= hashSet.size -> acc *)
(*        | _ -> collect_all (i + 1) (collect_chain acc hashSet.chains.(i)) *)
(*      in *)
(*      let chain = collect_all 0 Empty in *)
(*      let rec map_node = function *)
(*              | Empty -> Empty *)
(*              | Node node -> Node {entry = {hash = hash (f node.entry.value); value = f node.entry.value}; next = map_node node.next} *)
(*      in *)
(*      let map_chain = map_node chain in *)
(*      let rec add_chain_to_set set = function *)
(*          | Empty -> set *)
(*          | Node node -> *)
(*              let set = add set node.entry.value in *)
(*              add_chain_to_set set node.next *)
(*        in *)
(*        let new_hashSet = { size = hashSet.size; chains = Array.make hashSet.size Empty } in *)
(*          add_chain_to_set new_hashSet map_chain *)

let map f hashSet =
  let rec map_chain acc = function
    | Empty -> acc
    | Node { entry; next } ->
        let new_entry = { hash = hash (f entry.value); value = f entry.value } in
        map_chain (add acc new_entry.value) next
  in
  Array.fold_left map_chain { size = hashSet.size; chains = Array.make hashSet.size Empty } hashSet.chains



(* let fold_left f acc hashSet = *)
(*  let rec fold_chain acc = function *)
(*    | Empty -> acc *)
(*    | Node {entry; next} -> fold_chain (f acc entry.value) next *)
(*  in *)
(*  Array.fold_left fold_chain acc hashSet.chains *)
(*  *)
(*  *)

let fold_left f init hashSet =
  let rec fold_chain acc = function
    | Empty -> acc
    | Node { entry; next } -> fold_chain (f acc entry.value) next
  in

  let rec fold_array acc i =
    match i with
    | i when i >= hashSet.size -> acc  (* Индекс выходит за пределы массива, возвращаем аккумулятор *)
    | i -> fold_array (fold_chain acc hashSet.chains.(i)) (i + 1)  (* Рекурсивно обрабатываем цепочку и идем к следующему элементу массива *)
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





