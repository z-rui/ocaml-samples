let two_pi = 2. *. Float.pi
let rad_to_deg x = x *. 180. /. Float.pi

(* Parameters *)
let num_balls = ref 10
let poly_radius = ref 200.
let poly_sides = ref 7
let period = ref 10.
let ball_radius = ref 10.
let screen_width = ref 640
let screen_height = ref 480
let target_fps = ref 60

let argspec =
  [
    ("-b", Arg.Set_int num_balls, "number of balls (default 10)");
    ("-s", Arg.Set_int poly_sides, "number of sides of polygon (default 7)");
    ( "-t",
      Arg.Set_float period,
      "period of polygon rotation in seconds (default 10)" );
    ("-R", Arg.Set_float poly_radius, "Polygon radius (default 200)");
    ("-r", Arg.Set_float ball_radius, "Ball radius (default 10)");
    ("-n", Arg.Set_int num_balls, "Number of balls (default 10)");
    ("-w", Arg.Set_int screen_width, "Screen width (default 640)");
    ("-h", Arg.Set_int screen_height, "Screen width (default 480)");
    ("-fps", Arg.Set_int target_fps, "Target frame per second (default 60)");
  ]

let () =
  Arg.parse argspec ignore "bouncing_balls";
  Random.self_init ();

  (* Setup simulator *)
  let poly =
    Simulator.Polygon.create ~radius:!poly_radius ~sides:!poly_sides
      ~angle_vel:(two_pi /. !period)
  in
  let balls =
    let safe_radius = Simulator.Polygon.inner_radius poly -. !ball_radius in
    Array.init !num_balls (fun _ ->
        let r, theta = Random.(float safe_radius, float two_pi) in
        let pos = Simulator.Vec2.(from_polar (r, theta)) in
        Simulator.Ball.create ~radius:!ball_radius ~pos)
  in
  let gravity = Simulator.Vec2.from_rect (0., 100.) in
  let sim = Simulator.(create ~gravity balls poly) in

  (* Setup graphics *)
  let open Raylib in
  set_config_flags ConfigFlags.[ Msaa_4x_hint ];
  init_window !screen_width !screen_height "Bouncing_balls demo";
  let center =
    Vector2.create
      (0.5 *. Float.of_int !screen_width)
      (0.5 *. Float.of_int !screen_height)
  in
  set_target_fps !target_fps;
  let frame_time = 1. /. Float.of_int !target_fps in

  (* Simulation and rendering *)
  while not (window_should_close ()) do
    Fun.protect ~finally:end_drawing begin fun () ->
        begin_drawing ();
        clear_background Color.black;
        let angle = Simulator.Polygon.angle poly in
        draw_poly center !poly_sides !poly_radius (rad_to_deg angle) Color.white;
        Array.iter
          (fun ball ->
            let x, y = Simulator.(Vec2.to_rect (Ball.pos ball)) in
            let p = Vector2.create x y in
            draw_circle_v Vector2.(add p center) !ball_radius Color.red)
          balls;
        draw_fps 0 0
      end;
    Simulator.advance ~dt:frame_time sim
  done
