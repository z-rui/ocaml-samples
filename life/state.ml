module Cells = struct
  type t = Bigarray.((int, int8_unsigned_elt, c_layout) Array2.t)

  let create rows cols =
    let cells : t = Bigarray.(Array2.create int8_unsigned c_layout rows cols) in
    Bigarray.Array2.fill cells 0;
    cells

  let get_alive (cells : t) i j =
    let encoded = cells.{i, j} in
    encoded land 0x80 <> 0

  let update_neighbors (t : t) i j inc =
    let rows, cols = Bigarray.Array2.(dim1 t, dim2 t) in
    List.iter2
      begin fun di dj ->
        let i' = (i + di + rows) mod rows and j' = (j + dj + cols) mod cols in
        t.{i', j'} <- t.{i', j'} + inc
      end
      [ -1; -1; -1; 0; 0; 1; 1; 1 ]
      [ -1; 0; 1; -1; 1; -1; 0; 1 ]

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
let get_alive t = Cells.get_alive @@ if t.phase then t.cells' else t.cells
let set_alive t = Cells.set_alive @@ if t.phase then t.cells' else t.cells

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
