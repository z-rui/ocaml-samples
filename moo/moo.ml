exception MalformedInput of string

let parse_input s =
  let used = Array.init 10 (Fun.const false) in
  let parsed = Array.init (String.length s) (Fun.const 0) in
  String.iteri
    begin fun i c ->
      match c with
      | '0' .. '9' ->
          let x = Char.code c - Char.code '0' in
          if used.(x) then raise (MalformedInput "duplicate digits");
          used.(x) <- true;
          parsed.(i) <- x
      | _ -> raise (MalformedInput "a non-digit")
    end
    s;
  parsed

let generate_key len =
  if len < 0 || len > 10 then invalid_arg "len";
  let used = Array.init 10 (Fun.const false) in
  let rec gen () =
    let x = Random.int 10 in
    if used.(x) then gen ()
    else begin
      used.(x) <- true;
      x
    end
  in
  Array.init len (fun _ -> gen ())

let key_to_str key =
  let buf = Bytes.create (Array.length key) in
  Array.iteri (fun i x -> Bytes.set buf i (Char.chr (Char.code '0' + x))) key;
  Bytes.unsafe_to_string buf

let check_input key input =
  let len = Array.length key in
  if Array.length input <> len then raise (MalformedInput "bad input length");
  let a = ref 0 and b = ref 0 in
  Array.iter2
    (fun x y -> if x = y then incr a else if Array.mem y key then incr b)
    key input;
  (!a, !b)

let game len =
  let key = generate_key len in
  let rec loop steps =
    match check_input key (parse_input (read_line ())) with
    | exception MalformedInput what ->
        Printf.printf "Sorry, I didn't understand.  I saw %s.\n" what;
        loop steps
    | exception End_of_file ->
        Printf.printf "Quit after %d steps.\nThe correct answer is %s.\n"
          (steps - 1) (key_to_str key);
        ()
    | a, b ->
        Printf.printf "%dA%dB\n" a b;
        if a = len then Printf.printf "Won after %d steps.\n" steps
        else loop (steps + 1)
  in
  loop 1

let () =
  Random.self_init ();
  game 4
