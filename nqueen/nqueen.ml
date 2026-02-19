open Domainslib

let nqueen_solver pool n =
  let allbits = (1 lsl n) - 1 in
  let count = Atomic.make 0 in
  let rec search ~multiplier cols ld rd =
    if cols = allbits then ignore (Atomic.fetch_and_add count multiplier)
    else
      let avail = ref (allbits land lnot (cols lor ld lor rd)) in
      while !avail <> 0 do
        let pos = !avail land - !avail in
        avail := !avail lxor pos;
        search ~multiplier (cols lor pos)
          ((ld lor pos) lsl 1)
          ((rd lor pos) lsr 1)
      done
  in
  let finish = (n - 1) / 2 in
  Task.parallel_for ~start:0 ~finish
    ~body:begin fun i ->
      let pos = 1 lsl i in
      search
      (* Middle position on first row counts as 1;
       * Every other position counts as 2 due to symmetry *)
        ~multiplier:(if n mod 2 = 1 && i = finish then 1 else 2)
        pos (pos lsl 1) (pos lsr 1)
    end
    pool;
  Atomic.get count

let () =
  let n =
    match Sys.argv with
    | [| _ |] -> 8
    | [| _; arg1 |] -> int_of_string arg1
    | _ -> failwith "wrong number of arguments; expect 0 or 1"
  in
  let num_domains = Domain.recommended_domain_count () in
  Printf.eprintf "solving n=%d with %d domains\n%!" n num_domains;
  let pool = Task.setup_pool ~num_domains () in
  let total = Task.run pool (fun () -> nqueen_solver pool n) in
  Printf.printf "%d\n" total
