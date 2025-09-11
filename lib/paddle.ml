(* lib/paddle.ml *)

(* --- PADDLE Module Implementation --- *)
(* This is the actual code for the paddle. It contains the data structure
   and the logic for movement and rendering. We are implementing the
   PADDLE signature defined above. *)
module Paddle = struct
  (* The internal type for our paddle. We use a record to hold its
     position and dimensions. This type is "abstract" to outside modules
     because it's not exposed in the signature. *)
  type t = {
    mutable position : Raylib.Vector2.t;
    width : float;
    height : float;
    speed : float;
  }

  (* Creates a new paddle instance at a given position. *)
  let create x y =
    {
      position = Raylib.Vector2.create x y;
      width = 100.0;
      height = 20.0;
      speed = 500.0;
    }

  let move delta_time direction screen_width paddle =
    let old_x = Raylib.Vector2.x paddle.position in
    let old_y = Raylib.Vector2.y paddle.position in
    let new_x =
      match direction with
      | 1 -> old_x +. (paddle.speed *. delta_time)
      | -1 -> old_x -. (paddle.speed *. delta_time)
      | _ -> old_x
    in
    let clamp v min_v max_v = max min_v (min v max_v) in
    let new_x_clamped = clamp new_x 0.0 (screen_width -. paddle.width) in
    let new_pos = Raylib.Vector2.create new_x_clamped old_y in

    { paddle with position = new_pos }

  (* Updates the paddle's position based on user input (left/right arrow keys).
     It also constrains the paddle within the window boundaries. *)

  let update delta_time screen_width paddle =
    let new_x =
      if Raylib.is_key_down Raylib.Key.Left then
        Raylib.Vector2.x paddle.position -. (paddle.speed *. delta_time)
      else if Raylib.is_key_down Raylib.Key.Right then
        Raylib.Vector2.x paddle.position +. (paddle.speed *. delta_time)
      else Raylib.Vector2.x paddle.position
    in
    (* Clamp the new position to stay within the screen boundaries. *)
    let clamp v min_v max_v = max min_v (min v max_v) in
    let new_x_clamped = clamp new_x 0.0 (screen_width -. paddle.width) in
    paddle.position <-
      Raylib.Vector2.create new_x_clamped (Raylib.Vector2.y paddle.position);
    paddle

  (* Draws the paddle as a rectangle on the screen. *)
  let draw paddle =
    Raylib.draw_rectangle_v paddle.position
      (Raylib.Vector2.create paddle.width paddle.height)
      Raylib.Color.blue

  (* Accessor functions to get the paddle's properties. These are exposed
     in the signature. *)
  let width paddle = paddle.width
  let height paddle = paddle.height
  let position paddle = paddle.position
end
