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

let update t =
  let new_position = Raylib.Vector2.add t.position t.speed in
  let speed_x =
    if
      Raylib.Vector2.x new_position
      >= Float.of_int (Raylib.get_screen_width () - t.radius)
      || Raylib.Vector2.x new_position <= Float.of_int t.radius
    then Raylib.Vector2.x t.speed *. -1.
    else Raylib.Vector2.x t.speed
  in
  let speed_y =
    if
      Raylib.Vector2.y new_position
      >= Float.of_int (Raylib.get_screen_height () - t.radius)
      || Raylib.Vector2.y new_position <= Float.of_int t.radius
    then Raylib.Vector2.y t.speed *. -1.
    else Raylib.Vector2.y t.speed
  in
  let speed = Raylib.Vector2.create speed_x speed_y in
  let ball = { t with position = new_position; speed } in
  ball

let maybe_hit_by_paddle t (*paddle*) =
  (* let h = Paddle.height paddle in
     let w = Paddle.width paddle in *)
  t
