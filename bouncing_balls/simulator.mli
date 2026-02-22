(** Vector operations module.

    This module provides basic 2D vector operations including conversion between
    rectangular and polar coordinates. *)
module Vec2 : sig
  type t
  (** The type representing a 2D vector. *)

  val from_rect : float * float -> t
  (** Create a vector from rectangular coordinates (x, y). *)

  val to_rect : t -> float * float
  (** Convert a vector to rectangular coordinates (x, y). *)

  val from_polar : float * float -> t
  (** Create a vector from polar coordinates (radius, angle in radians). *)

  val to_polar : t -> float * float
  (** Convert a vector to polar coordinates (radius, angle in radians). *)
end

(** Ball physics module.

    This module represents balls with position and velocity that can be affected
    by forces. *)
module Ball : sig
  type t
  (** The type representing a ball with physical properties. *)

  val create : radius:float -> pos:Vec2.t -> t
  (** Create a new ball with given radius and initial position.
      @param radius The radius of the ball in simulation units
      @param pos The initial position of the ball as a Vec2.t *)

  val pos : t -> Vec2.t
  (** Get the current position of the ball. *)
end

(** Polygon physics module.

    This module represents polygons that can rotate and serve as boundaries for
    balls. The polygon is assumed to be centered at the origin. *)
module Polygon : sig
  type t
  (** The type representing a rotating polygon boundary. *)

  val create : radius:float -> sides:int -> angle_vel:float -> t
  (** Create a new polygon with given properties.
      @param radius The distance from center to vertices
      @param sides The number of sides in the polygon
      @param angle_vel The angular velocity (radians per second) *)

  val angle : t -> float
  (** Get the current rotation angle of the polygon in radians. *)

  val inner_radius : t -> float
  (** Get the inner radius of the polygon, which is the maximum distance from
      center to any point inside the polygon. *)
end

type t
(** The main simulator type that manages the physics simulation. *)

val create :
  gravity:Vec2.t ->
  ?ball_ball_recovery_coeff:float ->
  ?ball_poly_recovery_coeff:float ->
  Ball.t array ->
  Polygon.t ->
  t
(** Create a new physics simulator.
    @param gravity The gravitational acceleration vector applied to all balls
    @param ball_ball_recovery_coeff
      Optional coefficient for ball-ball collisions (default: 0.9)
    @param ball_poly_recovery_coeff
      Optional coefficient for ball-polygon collisions (default: 0.8)
    @param balls Array of balls to include in the simulation
    @param poly The polygon boundary that balls will collide with *)

val advance : dt:float -> t -> unit
(** Advance the simulation by a time step.
    @param dt The time delta to advance the simulation (in seconds) *)
