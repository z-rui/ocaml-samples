type wc_stats = { lines : int; words : int; chars : int }

let run_wc in_chan =
  let rec iter in_word lines words chars =
    match In_channel.input_char in_chan with
    | None -> { lines; words; chars }
    | Some c -> (
        match c with
        | ' ' | '\t' | '\n' | '\r' | '\011' | '\012' ->
            iter false (if c = '\n' then lines + 1 else lines) words (chars + 1)
        | _ -> iter true lines (if in_word then words else words + 1) (chars + 1)
        )
  in
  iter false 0 0 0

let print_lines = ref false
let print_words = ref false
let print_chars = ref false
let enable_flags = [| print_lines; print_words; print_chars |]
let results = Queue.create ()

let proc_file filename in_chan =
  if not (Array.exists ( ! ) enable_flags) then
    Array.iter (fun en -> en := true) enable_flags;
  let stats = run_wc in_chan in
  let filtered_stats =
    List.filteri
      (fun i _ -> !(enable_flags.(i)))
      [ stats.lines; stats.words; stats.chars ]
  in
  Queue.push (filtered_stats, filename) results

let print_results results =
  let width =
    Queue.fold
      (fun acc (stats, filename) ->
        Int.max acc
          (match filename with
          | None -> 1000000
          | Some _ -> List.fold_left Int.max 0 stats))
      0 results
    |> Int.to_string |> String.length
  in
  Queue.iter
    (fun (stats, filename) ->
      let stats_str =
        List.map (Printf.sprintf "%*d" width) stats |> String.concat " "
      in
      match filename with
      | None -> Printf.printf "%s\n" stats_str
      | Some filename -> Printf.printf "%s %s\n" stats_str filename)
    results

let argspec =
  [
    ("-l", Arg.Set print_lines, "print the newline counts");
    ("-w", Arg.Set print_words, "print the word counts");
    ("-c", Arg.Set print_chars, "print the byte counts");
  ]

let () =
  Arg.parse argspec
    (fun arg -> In_channel.with_open_bin arg (proc_file (Some arg)))
    "wc [-l] [-w] [-c] <file1> [<file2> ...]";
  if Queue.is_empty results then proc_file None In_channel.stdin;
  print_results results
