(* This program reads from the standard input and writes to standard output.
 * Examples of use:
 *    $ ocaml fasta.ml < fasta_file.txt
 *    $ ocaml fasta.ml < fasta_file.txt > my_result.txt
 *
 * The FASTA file is assumed to have a specific format, where the first line
 * contains a label in the form of '>blablabla', i.e. with a '>' as the first
 * character.
 *)

let labelstart = '>'

let is_label s = s.[0] = labelstart
let get_label s = String.sub s 1 (String.length s - 1)

let read_in channel = input_line channel |> String.trim

let print_fasta chan =
  let rec doloop currlabel line =
    match line with
    | s when is_label s ->
        let newlabel = get_label line
        and _ = if currlabel <> "" then print_newline () else () in
        let _ = print_string (newlabel ^ ": ") in
        doloop newlabel (read_in chan)
    | _ ->
        let _ = print_string line in
        doloop currlabel (read_in chan)
  in
  try
    match read_in chan with
    | line when is_label line -> doloop "" line
    | _ -> failwith "Badly formatted FASTA file?"
  with
    End_of_file -> print_newline ()


let () =
  print_fasta stdin

