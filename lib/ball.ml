type t = {
  position : Raylib.Vector2.t;
  speed : Raylib.Vector2.t;
  radius : float;
}

let draw (ball : t) =
  let open Raylib in
  draw_circle_v ball.position ball.radius Color.gray

(* Creates a new ball instance at a given position. *)
let create x y =
  {
    position = Raylib.Vector2.create x y;
    radius = 10.0;
    speed = Raylib.Vector2.create 5.0 4.0;
  }

let update t =
  let new_position = Raylib.Vector2.add t.position t.speed in
  let speed_x =
    if
      Raylib.Vector2.x new_position
      >= Float.of_int (Raylib.get_screen_width ()) -. t.radius
      || Raylib.Vector2.x new_position <= t.radius
    then Raylib.Vector2.x t.speed *. -1.
    else Raylib.Vector2.x t.speed
  in
  let speed_y =
    if
      Raylib.Vector2.y new_position
      >= Float.of_int (Raylib.get_screen_height ()) -. t.radius
      || Raylib.Vector2.y new_position <= t.radius
    then Raylib.Vector2.y t.speed *. -1.
    else Raylib.Vector2.y t.speed
  in
  let speed = Raylib.Vector2.create speed_x speed_y in
  let ball = { t with position = new_position; speed } in
  ball

let not_in_y_range ball paddle =
  let ball_y = Raylib.Vector2.y ball.position in
  let paddle_y = Raylib.Vector2.y (Paddle.position paddle) in
  let paddle_h = Paddle.height paddle in

  ball_y +. ball.radius < paddle_y
  || ball_y -. ball.radius > paddle_y +. paddle_h

let not_in_x_range ball paddle =
  let ball_x = Raylib.Vector2.x ball.position in
  let paddle_x = Raylib.Vector2.x (Paddle.position paddle) in
  let paddle_w = Paddle.width paddle in

  ball_x +. ball.radius < paddle_x
  || ball_x -. ball.radius > paddle_x +. paddle_w

let reverse_y_speed ball rel_hit_pos =
  let speed_y = Raylib.Vector2.y ball.speed in
  let speed_x = Raylib.Vector2.x ball.speed in
  let x_chg = (rel_hit_pos -. 0.5) *. 2.0 in
  let speed_x = speed_x *. x_chg in
  print_float speed_x;
  print_endline "";
  { ball with speed = Raylib.Vector2.create speed_x (speed_y *. -1.0) }

let maybe_hit_by_paddle ball paddle =
  if Raylib.Vector2.y ball.speed <= 0. then ball
    (* Only check on the way down *)
  else if not_in_y_range ball paddle then ball
  else if not_in_x_range ball paddle then ball
  else
    let paddle_hit_rel = Paddle.paddle_hit_rel paddle ball.position in
    reverse_y_speed ball paddle_hit_rel
