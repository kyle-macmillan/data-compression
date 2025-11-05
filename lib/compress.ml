type algorithm = 
| Prefix_free_tree
| Prefix_free_naive

let compress ~alg data =
  match alg with
  | _ -> Prefix_free.compress data

let decompress ~alg data =
  match alg with
  | Prefix_free_tree -> Prefix_free_tree.decompress data
  | Prefix_free_naive -> Prefix_free_naive.decompress data
