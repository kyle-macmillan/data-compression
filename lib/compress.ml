open Base

type algorithm = 
| Prefix_free_tree
| Prefix_free_naive

let compress ~alg data =
  match alg with
  | _ -> Prefix_free_encode.compress data

let decompress ~alg data =
  match alg with
  | Prefix_free_tree -> Prefix_free_tree_decode.decompress data
  | Prefix_free_naive -> Prefix_free_naive_decode.decompress data

let%test_module "Mirror Tests" = (module struct

  let run_mirror_test ~alg word =
    let encoded = compress ~alg word in
    let decoded = decompress ~alg encoded in
    String.equal word decoded

  let%test "Prefix free naive mirror" = 
    run_mirror_test ~alg:Prefix_free_naive "ABCD"

  let%test "Prefix_free_naive roundtrip (complex)" =
    run_mirror_test ~alg:Prefix_free_naive "AABBCDAACDB"

  (* The test for the Tree algorithm *)
  let%test "Prefix_free_tree roundtrip" =
    run_mirror_test ~alg:Prefix_free_tree "ABCD"

  let%test "Prefix_free_tree roundtrip (complex)" =
    run_mirror_test ~alg:Prefix_free_tree "AABBCDAACDB"

  let%test "Prefix_free_tree roundtrip (empty)" =
    run_mirror_test ~alg:Prefix_free_tree ""
end)
