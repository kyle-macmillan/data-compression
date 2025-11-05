type algorithm = 
| Prefix_free_tree
| Prefix_free_naive

val compress : alg:algorithm -> string -> string
val decompress : alg:algorithm -> string -> string
