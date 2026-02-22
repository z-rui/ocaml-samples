module Vec2 : sig
  type t

  val from_rect : float * float -> t
  val to_rect : t -> float * float
  val from_polar : float * float -> t
  val to_polar : t -> float * float
end

module Ball : sig
  type t

  val create : radius:float -> pos:Vec2.t -> t
  val pos : t -> Vec2.t
end

module Polygon : sig
  type t

  val create : radius:float -> sides:int -> angle_vel:float -> t
  val angle : t -> float
  val inner_radius : t -> float
end

type t

val create :
  gravity:Vec2.t ->
  ?ball_ball_recovery_coeff:float ->
  ?ball_poly_recovery_coeff:float ->
  Ball.t array ->
  Polygon.t ->
  t

val advance : dt:float -> t -> unit
