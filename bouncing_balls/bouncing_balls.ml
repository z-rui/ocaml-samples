let two_pi = 2. *. Float.pi
let rad_to_deg x = x *. 180. /. Float.pi

let setup ~period ~poly_radius ~poly_sides ~ball_radius ~num_balls =
  let open Simulator in
  let poly =
    Polygon.create ~radius:poly_radius ~sides:poly_sides
      ~angle_vel:(two_pi /. period)
  in
  let balls =
    let safe_radius = Polygon.inner_radius poly -. ball_radius in
    Array.init num_balls (fun _ ->
        let r, theta = Random.(float safe_radius, float two_pi) in
        let pos = Vec2.(from_polar (r, theta)) in
        Ball.create ~radius:ball_radius ~pos)
  in
  (balls, poly)

(* Parameters *)
let num_balls = ref 10
let poly_radius = ref 200.
let poly_sides = ref 7
let period = ref 10.
let ball_radius = ref 10.

let argspec =
  [
    ("-b", Arg.Set_int num_balls, "number of balls (default 10)");
    ("-s", Arg.Set_int poly_sides, "number of sides of polygon (default 7)");
    ( "-t",
      Arg.Set_float period,
      "period of polygon rotation in seconds (default 10)" );
    ("-R", Arg.Set_float poly_radius, "Polygon radius (default 200.)");
    ("-r", Arg.Set_float ball_radius, "Ball radius (default 10)");
    ("-n", Arg.Set_int num_balls, "Number of balls (default 10)");
  ]

let () =
  Arg.parse argspec ignore "bouncing_balls";
  Random.self_init ();

  let balls, poly =
    setup ~period:!period ~poly_radius:!poly_radius ~poly_sides:!poly_sides
      ~ball_radius:!ball_radius ~num_balls:!num_balls
  in
  let gravity = Simulator.Vec2.from_rect (0., 100.) in
  let sim = Simulator.(create ~gravity balls poly) in
  let target_fps = 60 in

  (* Setup graphics *)
  let open Raylib in
  set_config_flags [ ConfigFlags.Msaa_4x_hint ];
  init_window 640 480 "Bouncing_balls demo";
  let center = Vector2.create 320. 240. in
  set_target_fps target_fps;
  let frame_time = 1. /. Float.of_int target_fps in

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
