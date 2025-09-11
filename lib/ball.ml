type t = { position : Raylib.Vector2.t; speed : Raylib.Vector2.t; radius : int }

let draw (ball : t) =
  let open Raylib in
  draw_circle_v ball.position (Float.of_int ball.radius) Color.gray
