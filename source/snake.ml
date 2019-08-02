(*
 * snake.ml
 * Jonathan Lowe
 * 2019/07/31
 *
 * This file contains an implementation of the game snake using the Graphics
 * ocaml library for the gui and Unix module for waiting between frames.
 *)

open Graphics

(*
 * Exceptions and types used
 *)
exception Snake_out_of_bounds

exception Snake_hit_itself

type snake_block = Empty | Apple | Snake of int

type motion = Up | Down | Left | Right | Stationary

type game_state = {
  head_x : int;
  head_y : int;
  max_length : int;
  motion : motion;
  grid : snake_block array array;
}

(*
 * Variables for keeping track of various sizes for the playing field.
 *)
let grid_width = 40

let grid_height = 40

let px_width = 850

let px_height = 850

let cell_width = px_width / grid_width

let cell_height = px_height / grid_height

let cell_margin = 3

(*
 * Rendering functions
 *)

let draw_cell x_offset y_offset color x y =
  set_color color ;
  fill_rect
    ((x * cell_width) + x_offset + cell_margin)
    ((y * cell_height) + y_offset + cell_margin)
    (cell_width - cell_margin)
    (cell_height - cell_margin)

let draw_frame state =
  (* Fills background color *)
  set_color black ;
  fill_rect 0 0 (size_x ()) (size_y ()) ;
  (* Draws a border around the playing field *)
  let x_offset = (size_x () - px_width) / 2 in
  let y_offset = (size_y () - px_height) / 2 in
  set_color white ;
  draw_rect
    (x_offset - 1 - cell_margin)
    (y_offset - 1 - cell_margin)
    (px_width + 2 + (2 * cell_margin))
    (px_height + 2 + (2 * cell_margin)) ;
  (* Draws all the cells in the playing field *)
  let draw_cell = draw_cell x_offset y_offset in
  Array.iteri
    (fun x column ->
      Array.iteri
        (fun y cell ->
          match cell with
          | Empty -> ()
          | Apple -> draw_cell red x y
          | Snake _ -> draw_cell white x y)
        column)
    state.grid

let place_apple grid =
  (* Get possible locations to place the apple *)
  let _, possible_locations =
    Array.fold_left
      (fun (x, acc) column ->
        let _, acc =
          Array.fold_left
            (fun (y, acc) cell ->
              match cell with
              | Empty -> (y + 1, (x, y) :: acc)
              | Apple -> (y + 1, acc)
              | Snake _ -> (y + 1, acc))
            (0, acc) column
        in
        (x + 1, acc))
      (0, []) grid
  in
  (* Randomly chooses place to place the apple *)
  if List.length possible_locations > 0 then
    let placement_idx = Random.int (List.length possible_locations) in
    let x, y = List.nth possible_locations placement_idx in
    grid.(x).(y) <- Apple

let move_snake state =
  (* In charge of removing the end of the snake's tail *)
  Array.iteri
    (fun x column ->
      Array.iteri
        (fun y cell ->
          match (cell, state.motion) with
          | _, Stationary | Empty, _ | Apple, _ -> ()
          | Snake pos, _ ->
              state.grid.(x).(y) <-
                (if pos < state.max_length then Snake (pos + 1) else Empty))
        column)
    state.grid ;
  (* Moves the snake's location corresponding to its direction *)
  let head_x, head_y =
    match state.motion with
    | Up -> (state.head_x, state.head_y + 1)
    | Down -> (state.head_x, state.head_y - 1)
    | Right -> (state.head_x + 1, state.head_y)
    | Left -> (state.head_x - 1, state.head_y)
    | Stationary -> (state.head_x, state.head_y)
  in
  (* Ensures that the snake doesn't go out of bounds *)
  if head_y >= grid_height then raise Snake_out_of_bounds ;
  if head_y < 0 then raise Snake_out_of_bounds ;
  if head_x >= grid_width then raise Snake_out_of_bounds ;
  if head_x < 0 then raise Snake_out_of_bounds ;
  (* Eat the apple if it is there and check to ensure it doesn't hit itself *)
  let max_length =
    match (state.motion, state.grid.(head_x).(head_y)) with
    | Stationary, _ | _, Empty -> state.max_length
    | _, Apple -> place_apple state.grid ; state.max_length + 1
    | _, Snake _ -> raise Snake_hit_itself
  in
  (* Moves snake's head in the grid *)
  state.grid.(head_x).(head_y) <- Snake 0 ;
  {state with head_x; head_y; max_length}

let rec main_loop state =
  let rec get_last_keypress key =
    if key_pressed () then get_last_keypress (read_key ()) else key
  in
  let state =
    if key_pressed () then
      match read_key () with
      | '\027' ->
          print_endline "Escape key pressed exiting" ;
          raise Exit
      | key ->
          (* Move in the direction of the keypress except ignore keypresses in
           * the opposite direction of motion. *)
          {
            state with
            motion =
              ( match (state.motion, get_last_keypress key) with
              | Right, '\037' | Right, 'h' | Right, 'H' -> Right
              | _, '\037' | _, 'h' | _, 'H' -> Left
              | Down, '\038' | Down, 'k' | Down, 'K' -> Down
              | _, '\038' | _, 'k' | _, 'K' -> Up
              | Left, '\039' | Left, 'l' | Left, 'L' -> Left
              | _, '\039' | _, 'l' | _, 'L' -> Right
              | Up, '\040' | Up, 'j' | Up, 'J' -> Up
              | _, '\040' | _, 'j' | _, 'J' -> Down
              | _, _ -> state.motion );
          }
    else state
  in
  let state = move_snake state in
  draw_frame state ; Unix.sleepf 0.1 ; main_loop state

let get_initial_state () =
  let grid = Array.make_matrix grid_width grid_height Empty in
  let _ = grid.(grid_width / 2).(grid_height / 2) <- Snake 0 in
  Random.self_init () ;
  place_apple grid ;
  {
    grid;
    motion = Stationary;
    max_length = 3;
    head_x = grid_width / 2;
    head_y = grid_height / 2;
  }

let _ = open_graph ""

let _ = main_loop (get_initial_state ())
