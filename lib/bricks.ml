module Brick = struct
  type t = { position : Raylib.Vector2.t }
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
            { Brick.position = Raylib.Vector2.create x y }))
  in
  { bricks; position = Raylib.Vector2.create 0.0 0.0 }

let draw bricks =
  let open Raylib in
  Array.iter
    (Array.iter (fun brick ->
         draw_rectangle_v brick.Brick.position
           (Vector2.create brick_width brick_height)
           Color.red))
    bricks.bricks

(* Return array row for a ball y value *)
let brick_row_of_y y = y

(* Return a new ball and a new bricks array after collision check *)
let maybe_hit_bricks (ball : Ball.t) bricks =
  let y = Raylib.Vector2.y ball.position in
  let row = brick_row_of_y y in
  if row = 0. then (ball, bricks) else (ball, bricks)
