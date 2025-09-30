module Brick = struct
  type t = { position : Raylib.Vector2.t; visible : bool }
end

type t = { bricks : Brick.t array array; position : Raylib.Vector2.t }

let brick_width = 60.0
let brick_col_width = brick_width +. 20.0
let brick_col_ofs = 35.0
let brick_height = 10.0
let brick_row_height = brick_height +. 10.0
let brick_row_ofs = 50.0

let create rows cols =
  let bricks =
    Array.init rows (fun row ->
        Array.init cols (fun col ->
            let x = (float_of_int col *. brick_col_width) +. brick_col_ofs in
            let y =
              (float_of_int row *. (brick_height +. brick_row_height))
              +. brick_row_ofs
            in
            { Brick.position = Raylib.Vector2.create x y; visible = true }))
  in
  { bricks; position = Raylib.Vector2.create 0.0 0.0 }

let draw bricks =
  let open Raylib in
  Array.iter
    (Array.iter (fun brick ->
         if brick.Brick.visible then
           draw_rectangle_v brick.Brick.position
             (Vector2.create brick_width brick_height)
             Color.red))
    bricks.bricks

let not_in_y_range (brick_row : Brick.t array) (ball : Ball.t) =
  let brick_y = Raylib.Vector2.y brick_row.(0).position in
  let ball_y = Raylib.Vector2.y ball.position in
  let min_y = brick_y -. ball.radius in
  let max_y = brick_y +. brick_height +. ball.radius in
  ball_y < min_y || ball_y > max_y

let not_in_x_range (brick : Brick.t) (ball : Ball.t) =
  let brick_x = Raylib.Vector2.x brick.position in
  let ball_x = Raylib.Vector2.x ball.position in
  let min_x = brick_x -. ball.radius in
  let max_x = brick_x +. brick_width +. ball.radius in
  ball_x < min_x || ball_x > max_x

(* Return array row for a ball y value *)
let brick_row_of_ball ball (bricks : Brick.t array array) =
  Array.find_index (fun el -> not (not_in_y_range el ball)) bricks

(* Return array col for a ball y value *)
let brick_col_of_ball ball (brick_row : Brick.t array) =
  Array.find_index (fun el -> not (not_in_x_range el ball)) brick_row

(* Return a new ball and a new bricks array after collision check *)
let maybe_hit_bricks (ball : Ball.t) bricks =
  let row = brick_row_of_ball ball bricks in
  match row with
  | None -> (ball, bricks)
  | Some r -> (
      (* Ball is in range of row r. Check what brick col might be hit*)
      let brick_row = bricks.(r) in
      let col = brick_col_of_ball ball brick_row in
      match col with
      | None -> (ball, bricks)
      | Some c ->
          (* Ball hits brick in r,c. See if it still there and in that case remove it
          and bounce the ball*)
          let brick = bricks.(r).(c) in
          if brick.visible then (
            bricks.(r).(c) <- { brick with visible = false };
            (Ball.reverse_y_speed ball 0.5, bricks))
          else (ball, bricks))
