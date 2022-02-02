open Game
open Player_model

let color_print_players str n =
  match n with
  | 1 -> ANSITerminal.print_string [ ANSITerminal.red ] str
  | 2 -> ANSITerminal.print_string [ ANSITerminal.blue ] str
  | 3 -> ANSITerminal.print_string [ ANSITerminal.yellow ] str
  | 4 -> ANSITerminal.print_string [ ANSITerminal.green ] str
  | _ -> failwith "not valid player"

let update player piece roll = update_player player roll piece
(* if piece_in_play_won p then take_pieces_outplay p piece else p *)

(* else if roll = 6 then put_pieces_inplay player piece else player *)
let roll () =
  print_string "\nPress enter to roll";
  match read_line () with
  | "quit!" -> exit 0
  | _ ->
      let num = 1 + Random.int 6 in
      print_string ("You rolled a " ^ string_of_int num);
      num

(* "\nYou rolled a " ^ (string_of_int roll) ^ *)
let rec piece roll player =
  print_string
    "\n\
     Which piece do you want to move? These are the pieces that you \
     can move: \n";
  print_endline
    (String.concat ", "
       (pieces_in_play (pieces_able_to_play player roll)));
  try
    match read_line () with
    | "quit!" -> exit 0
    | a ->
        if List.exists (fun x -> x = a) (pieces_in_play player) then
          int_of_string a
        else failwith "not valid piece"
  with
  | _ ->
      print_string "You don't have that piece, try again" |> fun _ ->
      piece roll player

let rec prompt_piece_in_play player =
  print_endline
    "\n\
     Which piece do you want to take out? These are your pieces not \
     currently NOT in play";
  print_endline (String.concat ", " (pieces_not_in_play player));
  try
    match read_line () with
    | "quit!" -> exit 0
    | a ->
        if List.exists (fun x -> x = a) (pieces_not_in_play player) then
          int_of_string a
        else failwith "not valid piece"
  with
  | _ ->
      print_string "You don't have that piece, try again" |> fun _ ->
      prompt_piece_in_play player

let six_prompt =
  "\n\
   A 6? That means you can do two things \n\
  \ A. Take out one of your pieces... \n\
  \ B. Move one of your pieces 6 places \n\
  \ \n\
  \ Which do you want to do? \n"

let six_count_prompt n =
  if n = 1 then
    print_endline
      "\n\
       A 6 means you can roll again! Be careful with your luck you \
       don't want to roll 3 6's in a row! If you do you will lose your \
       turn!"
  else if n = 2 then
    print_endline
      "\n\
      \ Another 6!! WOW you have some really good luck! Don't roll \
       another one or you lose it all!"
  else
    print_endline
      "\n Your luck was so good it was bad. You lose your turn :((((("

let rec six_rolled player =
  if player_in_play player && pieces_able_to_play player 6 <> [] then (
    print_endline six_prompt;
    six_helper player)
  else if pieces_able_to_play player 6 = [] then
    put_pieces_inplay (prompt_piece_in_play player) player
  else update player (piece 6 player) 6

and six_helper player =
  try
    match read_line () with
    | "quit!" -> exit 0
    | a ->
        if a = "A" || a = "a" then
          put_pieces_inplay (prompt_piece_in_play player) player
        else if a = "B" || a = "b" then update player (piece 6 player) 6
        else failwith "invalid input"
  with
  | _ ->
      print_string "Err, you can't do that." |> fun _ ->
      six_rolled player

let manage_6_stack_helper player num =
  if player_in_play player then
    if not (legal_moves_available player num) then
      print_string "\nYou can't make any valid moves\n" |> fun _ ->
      player
    else if num = 6 then six_rolled player
    else update player (piece num player) num
  else if num = 6 then put_pieces_inplay (next_piece player) player
  else print_string "\nYou have no pieces in play\n" |> fun _ -> player

let rec manage_6_stack player sixes roll =
  if sixes = 0 then manage_6_stack_helper player roll
  else
    let p = manage_6_stack_helper player 6 in
    manage_6_stack p (sixes - 1) roll

(* if sixes = 0 then update player (piece roll player) roll else if
   legal_moves_available player 6 then six_rolled player else if (not
   (legal_moves_available player 6)) && pieces_left player then
   put_pieces_inplay (next_piece player) player else manage_6_stack
   player (sixes - 1) roll *)

let rec six_runner player count =
  six_count_prompt count;
  if count = 3 then player
  else
    let r = roll () in
    if r = 6 then six_runner player (count + 1)
    else manage_6_stack player count r

let rec roll_all x lst =
  if List.length lst > 0 then
    color_print_players
      ("\n\nPlayer "
      ^ string_of_int ((List.length lst - (x + 1)) * -1)
      ^ "\n")
      ((List.length lst - (x + 1)) * -1);
  player_in_play_helper lst x

and player_in_play_helper lst x =
  match lst with
  | [] -> []
  | h :: t ->
      let num = roll () in
      if player_in_play h then
        if not (legal_moves_available h num) then
          print_string "\nYou can't make any valid moves\n" |> fun _ ->
          h :: roll_all x t
        else if num = 6 then
          let top = six_runner h 1 (* six_rolled h *) in
          top :: roll_all x t
        else
          let top = update h (piece num h) num in
          top :: roll_all x t
      else if num = 6 then
        let top =
          six_runner h 1
          (* put_pieces_inplay (next_piece h) h *)
        in
        top :: roll_all x t
      else
        print_string "\nYou have no pieces in play\n" |> fun _ ->
        h :: roll_all x t

let check_win players =
  List.fold_right ( || ) (List.map player_won players) false

let rec find x n = function
  | [] -> failwith "not here"
  | h :: t -> if h = x then n else find x (n + 1) t

let rec game_loop x players =
  if check_win players then
    print_endline
      ("\n\nWoohoo! Player "
      ^ string_of_int (find (winner players) 1 players)
      ^ " won!\n")
  else roll_all x players |> game_loop x

let play_game x =
  Random.init 10;
  (*//fix me *)
  create_players x |> game_loop x

exception NotEnoughPlayers

let rec main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nLet's Play Ludo!\n";
  print_endline "How many players will be playing?";
  print_string "> ";
  try
    match read_line () with
    | exception End_of_file -> ()
    | players ->
        print_endline
          "At any point during the game type \"quit!\" to leave game!";
        if int_of_string players > 1 then
          play_game (int_of_string players)
        else raise NotEnoughPlayers
  with
  | NotEnoughPlayers ->
      print_endline "You need more players, find some friends!"
  | _ -> print_endline "Enter a valid number, please"

let () = main ()
