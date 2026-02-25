let two_pi = 2. *. Float.pi

module Vec2 = struct
  (* type t = { mutable x : float; mutable y : float } *)
  include Raylib.Vector2

  let[@inline] from_rect (x, y) = create x y
  let[@inline] to_rect v = (x v, y v)

  let[@inline] to_polar v =
    let a, b = to_rect v in
    (hypot a b, atan2 b a)

  let[@inline] from_polar (r, theta) = create (r *. cos theta) (r *. sin theta)

  let[@inline] fma_scalar u v k =
    let x1, y1 = (x u, y u) in
    let x2, y2 = (x v, y v) in
    create (Float.fma k x2 x1) (Float.fma k y2 y1)
end

module Ball = struct
  type t = { radius : float; mutable pos : Vec2.t; mutable vel : Vec2.t }

  let radius b = b.radius
  let pos b = b.pos
  let vel b = b.vel
  let create ~radius ~pos = { radius; pos; vel = Vec2.zero () }
  let[@inline] accel dir amount b = b.vel <- Vec2.fma_scalar b.vel dir amount
  let[@inline] move dir amount b = b.pos <- Vec2.fma_scalar b.pos dir amount
  let[@inline] advance dt b = move b.vel dt b
end

module Polygon = struct
  type t = {
    radius : float;
    sides : int;
    mutable angle : float;
    angle_vel : float;
    alpha : float;
    inner_radius : float;
  }

  let create ~radius ~sides ~angle_vel =
    let alpha = two_pi /. Float.of_int sides in
    {
      radius;
      sides;
      angle = 0.;
      angle_vel;
      alpha;
      inner_radius = radius *. cos (0.5 *. alpha);
    }

  let radius p = p.radius
  let sides p = p.sides
  let angle p = p.angle
  let inner_radius p = p.inner_radius

  let advance dt p =
    p.angle <- mod_float (Float.fma p.angle_vel dt p.angle) two_pi

  let to_boundary p pos =
    let r, theta = Vec2.to_polar pos in
    let i = Float.floor ((two_pi +. theta -. p.angle) /. p.alpha) in
    let theta' = (p.alpha *. (i +. 0.5)) +. p.angle in
    (p.inner_radius -. (r *. cos (theta -. theta')), theta')

  let vel_at p pos = Vec2.(scale (create (-.y pos) (x pos)) p.angle_vel)
end

type t = {
  balls : Ball.t array;
  poly : Polygon.t;
  gravity : Vec2.t;
  ball_ball_recovery_coeff : float;
  ball_poly_recovery_coeff : float;
}

let create ~gravity ?(ball_ball_recovery_coeff = 0.9)
    ?(ball_poly_recovery_coeff = 0.8) balls poly =
  { balls; poly; gravity; ball_ball_recovery_coeff; ball_poly_recovery_coeff }

let handle_ball_ball_collision ~e b b' =
  let dpos = Vec2.subtract (Ball.pos b) (Ball.pos b') in
  let dist = sqrt (Vec2.dot_product dpos dpos) in
  let overlap = Ball.(radius b +. radius b') -. dist in
  if overlap > 0. && dist > 0. then begin
    let dir = Vec2.scale dpos (1. /. dist) in
    (* adjust position *)
    Ball.move dir overlap b;
    (* adjust velocity *)
    let u = Vec2.dot_product (Ball.vel b) dir in
    let u' = Vec2.dot_product (Ball.vel b') dir in
    Ball.accel dir (-.u +. (u' *. e)) b;
    Ball.accel dir (-.u' +. (u *. e)) b'
  end

let handle_ball_poly_collision ~e poly ball =
  let pos = Ball.pos ball in
  let dist, theta = Polygon.to_boundary poly pos in
  let clearance = dist -. Ball.radius ball in
  if clearance < 0. then begin
    let dir = Vec2.from_polar (1., theta) in
    (* adjust position *)
    Ball.move dir clearance ball;
    (* adjust velocity *)
    let contact_point = Vec2.fma_scalar pos dir dist in
    let wall_vel = Polygon.vel_at poly contact_point in
    let rel_vel = Vec2.subtract (Ball.vel ball) wall_vel in
    let u = Vec2.dot_product rel_vel dir in
    let du = -.u *. (1. +. e) in
    Ball.accel dir du ball
  end

let advance ~dt sim =
  let balls = sim.balls in
  Array.iter (Ball.accel sim.gravity dt) balls;
  Polygon.advance dt sim.poly;
  let last = Array.length balls - 1 in
  for i = 0 to last do
    let b = balls.(i) in
    for j = i + 1 to last do
      handle_ball_ball_collision ~e:sim.ball_ball_recovery_coeff b balls.(j)
    done
  done;
  Array.iter
    (handle_ball_poly_collision ~e:sim.ball_poly_recovery_coeff sim.poly)
    balls;
  Array.iter (Ball.advance dt) balls
