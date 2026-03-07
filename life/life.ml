let rows = ref 60
let cols = ref 80
let scale = ref 10
let fps = ref 15

let arg_spec =
  [
    ("-w", Arg.Set_int cols, "number of columns");
    ("-h", Arg.Set_int rows, "number of rows");
    ("-s", Arg.Set_int scale, "size of each cell, in pixels");
    ("-fps", Arg.Set_int fps, "target frames per second");
  ]

let () =
  Arg.parse arg_spec ignore "life [-w width] [-h height] [-s scale]";
  Random.self_init ();
  let rows, cols = (!rows, !cols) in
  let scale = !scale in
  Raylib.init_window (cols * scale) (rows * scale) "Conway's Game of Life";
  let camera =
    let z = Raylib.Vector2.zero () in
    Raylib.Camera2D.create z z 0. (float_of_int scale)
  in
  let state = State.create rows cols in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      if Random.int 8 = 0 then State.set_alive state i j true
    done
  done;
  Raylib.set_target_fps !fps;
  while not (Raylib.window_should_close ()) do
    let open Raylib in
    begin_drawing ();
    clear_background Color.blue;
    begin_mode_2d camera;
    for i = 0 to rows - 1 do
      for j = 0 to cols - 1 do
        if State.get_alive state i j then draw_pixel j i Color.white
      done
    done;
    end_mode_2d ();
    draw_fps 0 0;
    end_drawing ();
    State.next state
  done
