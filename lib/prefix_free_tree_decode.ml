open Core

type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree


let decompress word =
  let tree = Node (Leaf "A", Node (Leaf "B", Node (Leaf "C", Leaf "D"))) in
  let rec decode (encoding : char list) (subtree : 'a tree) =
    match subtree with
    | Leaf v -> (
        v ^ match encoding with [] -> "" | _ -> decode encoding tree)
    | Node (left, right) -> (
        match encoding with
        | [] -> ""
        | '0' :: rest -> decode rest left
        | _ :: rest -> decode rest right)
  in
  decode (String.to_list word) tree
