let benchmark ~name f =
  let start_time = Sys.time () in
  f ();
  let end_time = Sys.time () in
  let duration = end_time -. start_time in
  Printf.printf "\n%s: %fs\n%!" name duration

let fib_loop n cb =
  let rec loop x y n =
    if n > 0 then begin
      cb x;
      loop y (x + y) (n - 1)
    end
  in
  loop 0 1 n

let fib_closure () =
  let x, y = (ref 0, ref 1) in
  fun () ->
    let x' = !x in
    x := !y;
    y := x' + !y;
    x'

let fib_seq () =
  let rec next x y = Seq.Cons (x, fun _ -> next y (x + y)) in
  fun _ -> next 0 1

let fib_seq2 () = Seq.unfold (fun (x, y) -> Some (x, (y, x + y))) (0, 1)

type _ Effect.t += Yield : int -> unit Effect.t

let fib_effect n =
  let rec loop x y n =
    if n > 0 then begin
      Effect.perform (Yield x);
      loop y (x + y) (n - 1)
    end
  in
  loop 0 1 n

type 'a stream = Cons of 'a * 'a stream Lazy.t

let fib_lazy () =
  let rec next x y = Cons (x, lazy (next y (x + y))) in
  next 0 1

let () =
  let fib_n = 10000000 in
  let cb =
    if fib_n > 10 then ignore
    else fun x ->
      print_int x;
      print_char ' '
  in
  benchmark ~name:"loop" begin fun _ ->
      fib_loop fib_n cb
    end;
  benchmark ~name:"closure" begin fun _ ->
      let gen = fib_closure () in
      for i = 1 to fib_n do
        cb (gen ())
      done
    end;
  benchmark ~name:"seq" begin fun _ ->
      let gen = fib_seq () in
      Seq.take fib_n gen |> Seq.iter cb
    end;
  benchmark ~name:"seq2" begin fun _ ->
      let gen = fib_seq2 () in
      Seq.take fib_n gen |> Seq.iter cb
    end;
  benchmark ~name:"effect" begin fun _ ->
      try fib_effect fib_n
      with effect Yield x, k ->
        cb x;
        Effect.Deep.continue k ()
    end;
  benchmark ~name:"lazy" begin fun _ ->
      let rec loop n (Cons (x, (lazy rest))) =
        if n > 0 then begin
          cb x;
          loop (n - 1) rest
        end
      in
      loop fib_n (fib_lazy ())
    end
