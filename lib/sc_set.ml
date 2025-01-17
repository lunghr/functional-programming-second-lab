(**)
type 'a entry = { hash : int; value : 'a }
type 'a chain = Empty | Node of { entry : 'a entry; next : 'a chain }
type 'a hashSet = { chains : 'a chain array }

let maxOccupancy = 0.8

let rec string_of_chain = function
  | Empty -> ""
  | Node node ->
      let entry_str =
        Printf.sprintf "{hash = %d; value = %d}" node.entry.hash
          node.entry.value
      in
      entry_str ^ " -> " ^ string_of_chain node.next

let string_of_hashSet hashSet =
  Array.fold_left
    (fun acc chain -> acc ^ string_of_chain chain ^ "\n")
    "" hashSet.chains

let create () : 'a hashSet = { chains = Array.make 1 Empty }
let hash value = Hashtbl.hash value

let countElements chain =
  let rec aux acc = function
    | Empty -> acc
    | Node { next; _ } -> aux (acc + 1) next
  in
  aux 0 chain

let countSetLength hashSet =
  Array.fold_left (fun acc chain -> acc + countElements chain) 0 hashSet.chains

let countChains hashSet =
  Array.fold_left (fun acc _ -> acc + 1) 0 hashSet.chains

let collectChainElements chain =
  let rec aux acc = function
    | Empty -> List.rev acc
    | Node { entry; next } -> aux (entry.value :: acc) next
  in
  aux [] chain

let recalculateChains hashSet size =
  let new_chains = Array.make size Empty in
  let recalculateChain chain =
    let chain = collectChainElements chain in
    List.iter
      (fun el ->
        let key = hash el in
        let index = key mod size in
        let rec update_chain chain =
          match chain with
          | Empty -> Node { entry = { hash = key; value = el }; next = Empty }
          | Node node -> (
              match node.entry.hash with
              | k when k = key ->
                  Node { entry = { hash = key; value = el }; next = node.next }
              | _ -> Node { entry = node.entry; next = update_chain node.next })
        in
        new_chains.(index) <- update_chain new_chains.(index))
      chain
  in
  Array.iter recalculateChain hashSet.chains;
  { chains = new_chains }

let resize hashSet =
  let elementsAmount = countSetLength hashSet in
  let chainsAmount = max (countChains hashSet) 1 in
  let occupancy = float_of_int elementsAmount /. float_of_int chainsAmount in

  match occupancy with
  | n when n > maxOccupancy ->
      let newSize = chainsAmount * 2 in
      recalculateChains hashSet newSize
  | _ -> hashSet

let add hashSet value =
  let size = countChains hashSet in
  let key = hash value in
  let index = key mod size in

  let rec update_chain chain =
    match chain with
    | Empty -> Node { entry = { hash = key; value }; next = Empty }
    | Node node -> (
        match node.entry.hash with
        | k when k = key ->
            Node { entry = { hash = key; value }; next = node.next }
        | _ -> Node { entry = node.entry; next = update_chain node.next })
  in

  let new_chains =
    Array.mapi
      (fun i c ->
        if i = index then
          update_chain c
        else
          c)
      hashSet.chains
  in
  resize { chains = new_chains }

let remove hashSet value =
  let size = countChains hashSet in
  let key = hash value in
  let index = key mod size in
  let rec remove_from_chain = function
    | Empty -> Empty
    | Node { entry; next } when entry.hash = key -> next
    | Node { entry; next } -> Node { entry; next = remove_from_chain next }
  in
  {
    chains =
      Array.mapi
        (fun i chain ->
          if i = index then
            remove_from_chain chain
          else
            chain)
        hashSet.chains;
  }

let setHas hashSet value =
  let size = countChains hashSet in
  let key = hash value in
  let index = key mod size in
  let rec loop = function
    | Empty -> false
    | Node node when node.entry.hash = key -> true
    | Node node -> loop node.next
  in
  loop hashSet.chains.(index)

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
  resize { chains = Array.map filter_chain hashSet.chains }

let map f hashSet =
  let rec map_chain acc = function
    | Empty -> acc
    | Node { entry; next } ->
        let new_entry =
          { hash = hash (f entry.value); value = f entry.value }
        in
        map_chain (add acc new_entry.value) next
  in
  let hashSet =
    Array.fold_left map_chain
      { chains = Array.make (countChains hashSet) Empty }
      hashSet.chains
  in
  resize hashSet

let fold_left f init hashSet =
  let size = countChains hashSet in
  let rec fold_chain acc = function
    | Empty -> acc
    | Node { entry; next } -> fold_chain (f acc entry.value) next
  in

  let rec fold_array acc i =
    match i with
    | i when i >= size -> acc
    | i -> fold_array (fold_chain acc hashSet.chains.(i)) (i + 1)
  in

  fold_array init 0

let fold_right f init hashSet =
  let size = countChains hashSet in
  let rec fold_chain acc = function
    | Empty -> acc
    | Node { entry; next } -> fold_chain (f acc entry.value) next
  in

  let rec fold_array acc i =
    match i with
    | i when i >= size -> acc
    | i -> fold_array (fold_chain acc hashSet.chains.(i)) (i + 1)
  in

  fold_array init 0

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
let isEmpty hashSet = countSetLength hashSet = 0

let merge (set1 : 'a hashSet) (set2 : 'a hashSet) : 'a hashSet =
  let size = max (countChains set1) (countChains set2) in
  let newChains =
    Array.init size (fun i ->
        let chain1 =
          if i < countChains set1 then
            set1.chains.(i)
          else
            Empty
        in
        let chain2 =
          if i < countChains set2 then
            set2.chains.(i)
          else
            Empty
        in
        mergeChains chain1 chain2)
  in
  { chains = newChains }

let rec compare_chains chain1 chain2 =
  match (chain1, chain2) with
  | Empty, Empty -> true
  | Empty, _ | _, Empty -> false
  | Node { entry = e1; next = n1 }, Node { entry = e2; next = n2 } ->
      e1.hash = e2.hash && e1.value = e2.value && compare_chains n1 n2

let compare_sets (set1 : 'a hashSet) (set2 : 'a hashSet) : bool =
  if countChains set1 != countChains set2 then
    false
  else
    Array.for_all2 compare_chains set1.chains set2.chains

