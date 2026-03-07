module Cells = struct
  type t = Bigarray.((int, int8_unsigned_elt, c_layout) Array2.t)

  let create rows cols =
    let cells : t = Bigarray.(Array2.create int8_unsigned c_layout rows cols) in
    Bigarray.Array2.fill cells 0;
    cells

  let get_alive (cells : t) i j = cells.{i, j} land 0x80 <> 0

  let update_neighbors (t : t) i j inc =
    let rows, cols = Bigarray.Array2.(dim1 t, dim2 t) in
    let pred' x m = if x = 0 then m - 1 else x - 1 in
    let succ' x m =
      let x' = x + 1 in
      if x' = m then 0 else x'
    in
    let i_p = pred' i rows and i_n = succ' i rows in
    let j_p = pred' j cols and j_n = succ' j cols in
    let upd i j = t.{i, j} <- t.{i, j} + inc in
    upd i_p j_p;
    upd i_p j;
    upd i_p j_n;
    upd i j_p;
    upd i j_n;
    upd i_n j_p;
    upd i_n j;
    upd i_n j_n

  let set_alive (t : t) i j alive =
    let encoded = t.{i, j} in
    if alive <> (encoded land 0x80 <> 0) then begin
      update_neighbors t i j @@ if alive then 1 else -1;
      t.{i, j} <- encoded lxor 0x80
    end

  let next (t : t) i j = function
    | 0x03 (* birth condition *) ->
        update_neighbors t i j 1;
        t.{i, j} <- t.{i, j} lor 0x80
    | 0x80 | 0x81 | 0x84 | 0x85 | 0x86 | 0x87 | 0x88 (* death condition *) ->
        update_neighbors t i j (-1);
        t.{i, j} <- t.{i, j} land 0x7f
    | _ -> ()
end

type t = { cells : Cells.t; cells' : Cells.t; mutable phase : bool }

let create rows cols =
  {
    cells = Cells.create rows cols;
    cells' = Cells.create rows cols;
    phase = false;
  }

let dim t = Bigarray.Array2.(dim1 t.cells, dim2 t.cells)

let get_alive t i j =
  Cells.get_alive (if t.phase then t.cells' else t.cells) i j

let set_alive t i j v =
  Cells.set_alive (if t.phase then t.cells' else t.cells) i j v

let next t =
  let rows, cols = dim t in
  let cells, cells' =
    if t.phase then (t.cells', t.cells) else (t.cells, t.cells')
  in
  Bigarray.Array2.blit cells cells';
  t.phase <- not t.phase;
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      Cells.next cells' i j cells.{i, j}
    done
  done
