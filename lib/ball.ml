type t = { position : Raylib.Vector2.t; speed : Raylib.Vector2.t; radius : int }

let draw (ball : t) =
  let open Raylib in
  draw_circle_v ball.position (Float.of_int ball.radius) Color.gray

(* Creates a new ball instance at a given position. *)
let create x y =
  {
    position = Raylib.Vector2.create x y;
    radius = 10;
    speed = Raylib.Vector2.create 5.0 4.0;
  }
