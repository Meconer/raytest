let screen_width = 1024
let screen_height = 800
let target_fps = 60

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
  set_target_fps target_fps;
  { State.ball; State.paddle; pause = false; frames_counter = 0 }

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
          let new_ball = Gamelib.Ball.update state.ball in
          let state = { state with ball = new_ball } in
          let new_paddle =
            Gamelib.Paddle.update state.paddle (1.0 /. float_of_int target_fps)
          in

          { state with paddle = new_paddle }
      in
      State.draw state;
      loop state

let () = setup () |> loop
