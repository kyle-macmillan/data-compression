open Base

let encode_letter letter =
  match letter with
  | 'A' -> "0"
  | 'B' -> "10"
  | 'C' -> "110"
  | 'D' -> "111"
  | _ -> ""

let compress word = String.concat_map word ~f:encode_letter

