let display =
  Array.iter (fun row ->
      Array.iter (Printf.printf "%4d") row;
      print_newline ())

let knight_solver n =
  let moves =
    [ (-2, -1); (-2, 1); (-1, -2); (-1, 2); (1, -2); (1, 2); (2, -1); (2, 1) ]
  in
  let total = n * n in
  let state = Array.make_matrix n n 0 in
  let rec search i x y =
    if i = total then raise Exit
    else
      let try_move (dx, dy) =
        let x', y' = (x + dx, y + dy) in
        if 0 <= x' && x' < n then
          let open Array in
          let row = unsafe_get state x' in
          if 0 <= y' && y' < n && unsafe_get row y' = 0 then begin
            let i' = i + 1 in
            unsafe_set row y' i';
            search i' x' y';
            unsafe_set row y' 0
          end
      in
      List.iter try_move moves
  in
  state.(0).(0) <- 1;
  match search 1 0 0 with exception Exit -> Some state | _ -> None

let () =
  let n =
    match Sys.argv with
    | [| _ |] -> 8
    | [| _; arg1 |] -> int_of_string arg1
    | _ -> failwith "wrong number of arguments; expect 0 or 1"
  in
  match knight_solver n with
  | Some state -> display state
  | None -> print_endline "no solution!"
