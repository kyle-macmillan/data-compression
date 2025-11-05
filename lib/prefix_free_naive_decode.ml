open Base

let decompress word =
  let rec decode (encoding : char list) (prefix : string) =
    match prefix with
    | "0" -> "A" ^ decode encoding ""
    | "10" -> "B" ^ decode encoding ""
    | "110" -> "C" ^ decode encoding ""
    | "111" -> "D" ^ decode encoding ""
    | _ -> (
        match encoding with
        | [] -> ""
        | hd :: rest -> decode rest (prefix ^ String.of_char hd))
  in
  decode (String.to_list word) ""

