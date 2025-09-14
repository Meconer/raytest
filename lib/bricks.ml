module Brick = struct
  type t = { position : Raylib.Vector2.t }
end

type t = { bricks : Brick.t array array; position : Raylib.Vector2.t }

let brick_width = 60.0
let brick_height = 10.0

let create rows cols =
  let bricks =
    Array.init rows (fun row ->
        Array.init cols (fun col ->
            let x = (float_of_int col *. (brick_width +. 5.0)) +. 35.0 in
            let y = (float_of_int row *. (brick_height +. 5.0)) +. 50.0 in
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
