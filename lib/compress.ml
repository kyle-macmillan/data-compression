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

  let rec generate_rnd_msg (length: int) =
    if length = 0 then ""
    else match Random.int 4 with
    | 0 -> "A" ^ generate_rnd_msg (length - 1)
    | 1 -> "B" ^ generate_rnd_msg (length - 1)
    | 2 -> "C" ^ generate_rnd_msg (length - 1)
    | _ -> "D" ^ generate_rnd_msg (length - 1)

  let%test "Prefix free naive random" =
    run_mirror_test ~alg:Prefix_free_naive (generate_rnd_msg 20)

  let%test "Prefix free tree random" =
    run_mirror_test ~alg:Prefix_free_tree (generate_rnd_msg 20)

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
