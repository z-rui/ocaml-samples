open Bigarray

module Insn = struct
  type t =
    | Incp of int
    | Inc of int
    | Read
    | Write
    | JumpNZ of int
    | JumpZ of int
end

let compile src =
  let open Insn in
  let rec loop pos acc =
    let size = String.length src in
    let push_incp k = function
      | Incp n :: rest -> if n + k = 0 then rest else Incp (n + k) :: rest
      | acc -> Incp k :: acc
    in
    let push_inc k = function
      | Inc n :: rest -> if n + k = 0 then rest else Inc (n + k) :: rest
      | acc -> Inc k :: acc
    in
    if pos < size then
      (match src.[pos] with
        | '<' -> push_incp (-1) acc
        | '>' -> push_incp 1 acc
        | '-' -> push_inc (-1) acc
        | '+' -> push_inc 1 acc
        | ',' -> Read :: acc
        | '.' -> Write :: acc
        (* placeholders for offsets *)
        | '[' -> JumpZ 0 :: acc
        | ']' -> JumpNZ 0 :: acc
        | _ -> acc)
      |> loop (pos + 1)
    else
      let code = acc |> List.rev |> Array.of_list in
      let jmp_stack = Stack.create () in
      (* populate offsets for JumpZ/JumpNZ *)
      Array.iteri
        (fun pc -> function
          | JumpZ _ -> Stack.push pc jmp_stack
          | JumpNZ _ ->
              if Stack.is_empty jmp_stack then failwith "too many ]";
              let pc' = Stack.pop jmp_stack in
              code.(pc') <- JumpZ (pc - pc');
              code.(pc) <- JumpNZ (pc' - pc)
          | _ -> ())
        code;
      if not (Stack.is_empty jmp_stack) then failwith "missing ] at end";
      code
  in
  loop 0 []

let execute ~data_size code =
  let data = Array1.create int8_unsigned c_layout data_size in
  Array1.fill data 0;
  let pc = ref 0 in
  let ptr = ref 0 in
  let code_size = Array.length code in
  let load () = Array1.unsafe_get data !ptr in
  let store x = Array1.unsafe_set data !ptr x in
  while !pc < code_size do
    begin match (code.(!pc) : Insn.t) with
    | Incp n -> ptr := (!ptr + n + data_size) mod data_size
    | Inc n -> (load () + n) land 0xff |> store
    | Read -> (
        match In_channel.input_byte stdin with Some n -> store n | _ -> ())
    | Write ->
        Out_channel.(
          load () |> output_byte stdout;
          flush stdout)
    | JumpNZ n -> if load () <> 0 then pc := !pc + n
    | JumpZ n -> if load () = 0 then pc := !pc + n
    end;
    pc := !pc + 1
  done

let () =
  let src =
    match Sys.argv with
    | [| cmd; arg1 |] -> In_channel.(with_open_text arg1 input_all)
    | _ ->
        {|++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.|}
  in
  compile src |> execute ~data_size:65536
