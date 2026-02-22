let two_pi = 2. *. Float.pi

module Vec2 = struct
  type t = { mutable x : float; mutable y : float }

  let[@inline] to_rect { x; y } = (x, y)
  let[@inline] from_rect (x, y) = { x; y }
  let[@inline] to_polar { x; y } = (hypot x y, atan2 y x)

  let[@inline] from_polar (r, theta) =
    { x = r *. cos theta; y = r *. sin theta }

  let[@inline] dotp u v = (u.x *. v.x) +. (u.y *. v.y)
  let[@inline] diff u v = { x = u.x -. v.x; y = u.y -. v.y }

  let[@inline] fma_scalar u v k =
    { x = Float.fma k v.x u.x; y = Float.fma k v.y u.y }

  let[@inline] scale_ ?out u k =
    let w = match out with None -> u | Some w -> w in
    w.x <- u.x *. k;
    w.y <- u.y *. k

  let[@inline] fma_scalar_ ?out u v k =
    let w = match out with None -> u | Some w -> w in
    w.x <- Float.fma k v.x u.x;
    w.y <- Float.fma k v.y u.y
end

module Ball = struct
  type t = { radius : float; pos : Vec2.t; vel : Vec2.t }

  let radius b = b.radius
  let pos b = b.pos
  let vel b = b.vel
  let create ~radius ~pos = { radius; pos; vel = { x = 0.; y = 0. } }
  let accel dir amount b = Vec2.fma_scalar_ b.vel dir amount
  let move dir amount b = Vec2.fma_scalar_ b.pos dir amount
  let advance dt b = Vec2.fma_scalar_ b.pos b.vel dt
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

  let vertex p i : Vec2.t =
    let r = p.radius in
    let theta = Float.fma p.alpha (float_of_int i) p.angle in
    Vec2.from_polar (r, theta)

  let to_boundary p pos =
    let r, theta = Vec2.to_polar pos in
    let i = Float.floor ((two_pi +. theta -. p.angle) /. p.alpha) in
    let theta' = (p.alpha *. (i +. 0.5)) +. p.angle in
    (p.inner_radius -. (r *. cos (theta -. theta')), theta')

  let vel_at p pos =
    let v = Vec2.{ x = -.pos.y; y = pos.x } in
    Vec2.scale_ v p.angle_vel;
    v
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

let adjust_velocity ~recovery_coeff ~dir ball =
  let v = Vec2.dotp (Ball.vel ball) dir in
  let dv = -.v *. (1. +. recovery_coeff) in
  Ball.accel dir dv ball

let handle_ball_ball_collision ~recovery_coeff (b : Ball.t) (b' : Ball.t) =
  let dpos = Vec2.(Ball.(diff (pos b) (pos b'))) in
  let dist = sqrt (Vec2.dotp dpos dpos) in
  let overlap = Ball.(radius b +. radius b') -. dist in
  if overlap > 0. && dist > 0. then begin
    Vec2.scale_ dpos (1. /. dist);
    (* adjust position *)
    Ball.move dpos overlap b;
    (* adjust velocity *)
    adjust_velocity ~recovery_coeff ~dir:dpos b;
    adjust_velocity ~recovery_coeff ~dir:dpos b'
  end

let handle_ball_poly_collision ~recovery_coeff poly ball =
  let pos = Ball.pos ball in
  let dist, theta = Polygon.to_boundary poly pos in
  let clearance = dist -. Ball.radius ball in
  if clearance < 0. then begin
    let dir = Vec2.from_polar (1., theta) in
    (* Adjust position *)
    Ball.move dir clearance ball;
    (* Adjust velocity *)
    let contact_point = Vec2.fma_scalar pos dir dist in
    let wall_vel = Polygon.vel_at poly contact_point in
    Ball.accel wall_vel (-1.) ball;
    (* ball's velocity is now relative to wall *)
    adjust_velocity ~recovery_coeff ~dir ball;
    Ball.accel wall_vel 1. ball
    (* ball's velocity is now relative to ground *)
  end

let advance ~dt sim =
  let balls = sim.balls in
  Array.iter (Ball.accel sim.gravity dt) balls;
  Polygon.advance dt sim.poly;
  let last = Array.length balls - 1 in
  Array.iteri
    begin fun i b ->
      for j = i + 1 to last do
        let recovery_coeff = sim.ball_ball_recovery_coeff in
        handle_ball_ball_collision ~recovery_coeff balls.(i) balls.(j)
      done
    end
    balls;
  Array.iter
    (handle_ball_poly_collision ~recovery_coeff:sim.ball_poly_recovery_coeff
       sim.poly)
    balls;
  Array.iter (Ball.advance dt) balls
