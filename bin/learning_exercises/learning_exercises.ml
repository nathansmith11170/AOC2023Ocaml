let rec rev ?(acc = []) = function [] -> acc | h :: t -> rev ~acc:(h :: acc) t
let rec is_palindrome list = rev list = list

type 'a node = One of 'a | Many of 'a node list

let rec flatten ?(acc = []) (list : 'a node list) =
  match list with
  | [] -> acc
  | One x :: t -> flatten ~acc:(acc @ [ x ]) t
  | Many l :: t -> flatten ~acc:(flatten ~acc l) t

let rec compress = function
  | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
  | smaller -> smaller

(* Accumulate consecutive identical members into sublists *)
let rec pack list =
  let rec aux current acc l =
    match l with
    (* if the list has more than 2 elements, compare the current to the next *)
    | curElem :: (nextElem :: _ as remaining) ->
        if curElem = nextElem then aux (current @ [ curElem ]) acc remaining
        else aux [] (acc @ [ current @ [ curElem ] ]) remaining
    (* with only one element left, determine if it belongs to the current sublist *)
    | [ last ] ->
        if current <> [] && List.hd current = last then
          acc @ [ current @ [ last ] ]
        else acc @ [ current ] @ [ [ last ] ]
    (* this is only reachable is the source is an empty list *)
    | [] -> []
  in
  aux [] [] list

let encode list =
  let rec aux count acc = function
    | [] -> [] (* Can only be reached if original list is empty *)
    | [ x ] -> acc @ [ (count + 1, x) ]
    | a :: (b :: _ as t) ->
        if a = b then aux (count + 1) acc t
        else aux 0 (acc @ [ (count + 1, a) ]) t
  in
  aux 0 [] list

type 'a rle = One of 'a | Many of int * 'a

let encode_m list =
  let create_tuple n e = if n = 1 then One e else Many (n, e) in
  let rec aux count acc = function
    | [] -> []
    | [ x ] -> acc @ [ create_tuple (count + 1) x ]
    | a :: (b :: _ as t) ->
        if a = b then aux (count + 1) acc t
        else aux 0 (acc @ [ create_tuple (count + 1) a ]) t
  in
  aux 0 [] list