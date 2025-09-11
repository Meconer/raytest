let screen_width = 1024
let screen_height = 800

module State = struct
  type t = {
    ball : Gamelib.Ball.t;
    paddle : Gamelib.Paddle.t;
    pause : bool;
    frames_counter : int;
  }

  let draw { ball; paddle; pause; frames_counter } =
    let open Raylib in
    begin_drawing ();
    clear_background Color.raywhite;
    Gamelib.Ball.draw ball;
    Gamelib.Paddle.draw paddle;

    draw_text "PRESS SPACE to PAUSE BALL MOVEMENT" 10
      (get_screen_height () - 25)
      20 Color.lightgray;

    if pause && frames_counter mod 60 > 30 then
      draw_text "Paused" 350 200 30 Color.gray;

    draw_fps 10 10;
    end_drawing ()
end

let setup () =
  let open Raylib in
  set_config_flags [ ConfigFlags.Msaa_4x_hint ];
  init_window screen_width screen_height
    "raylib [shapes] example - bouncing ball";

  let mid_x = float_of_int (get_screen_width () / 2) in
  let mid_y = float_of_int (get_screen_height () / 2) in
  let ball = Gamelib.Ball.create mid_x mid_y in

  let paddle_y = float_of_int (get_screen_height ()) *. 0.85 in
  let paddle = Gamelib.Paddle.create mid_x paddle_y in
  set_target_fps 60;
  { State.ball; State.paddle; pause = false; frames_counter = 0 }

let is_horizontal_wall_hit position radius =
  let open Raylib in
  Vector2.x position >= Float.of_int (get_screen_width () - radius)
  || Vector2.x position <= Float.of_int radius

let rec loop (state : State.t) =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
      let open Raylib in
      let state =
        if is_key_pressed Key.Space then { state with pause = not state.pause }
        else state
      in
      let state =
        if state.pause then
          { state with frames_counter = state.frames_counter + 1 }
        else
          let { Gamelib.Ball.position; radius; speed } = state.ball in
          let position = Vector2.add position speed in

          (* Check walls collision for bouncing *)
          let speed_x =
            if is_horizontal_wall_hit position radius then
              Vector2.x speed *. -1.0
            else Vector2.x speed
          in
          let speed_y =
            if
              Vector2.y position >= Float.of_int (get_screen_height () - radius)
              || Vector2.y position <= Float.of_int radius
            then Vector2.y speed *. -1.
            else Vector2.y speed
          in
          let speed = Vector2.create speed_x speed_y in
          let ball = { Gamelib.Ball.position; radius; speed } in
          let state = { state with ball } in
          let paddle = state.paddle in
          let move_paddle_dir =
            if is_key_down Key.Right then 1
            else if is_key_down Key.Left then -1
            else 0
          in
          let screen_width = Float.of_int (get_screen_width ()) in
          let paddle =
            Gamelib.Paddle.move 0.016 move_paddle_dir screen_width paddle
          in
          { state with paddle }
      in
      State.draw state;
      loop state

let () = setup () |> loop
