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

let roll _ =
  print_string "\nPress enter to roll";
  match read_line () with
  | _ ->
      let num = 1 + Random.int 6 in
      print_string ("You rolled a " ^ string_of_int num);
      num

let rec piece player =
  print_string
    "\n\
     Which piece do you want to move? These are your pieces in play: \n";
  print_endline (String.concat ", " (pieces_in_play player));
  try
    match read_line () with
    | a ->
        if List.exists (fun x -> x = a) (pieces_in_play player) then
          int_of_string a
        else failwith "not valid piece"
  with
  | _ ->
      print_string "You don't have that piece, try again" |> fun _ ->
      piece player

let rec prompt_piece_in_play player =
  print_endline
    "Which piece do you want to take out? These are your pieces not \
     currently NOT in play";
  print_endline (String.concat ", " (pieces_not_in_play player));
  try
    match read_line () with
    | a ->
        if List.exists (fun x -> x = a) (pieces_not_in_play player) then
          int_of_string a
        else failwith "not valid piece"
  with
  | _ ->
      print_string "You don't have that piece, try again" |> fun _ ->
      prompt_piece_in_play player

let rec six player =
  if player_in_play player then
    print_endline
      "\n\
       A 6? That means you can do two things \n\
      \ A. Take out one of your pieces... \n\
      \ B. Move one of your pieces 6 places \n\
      \ \n\
      \ Which do you want to do? \n";
  try
    match read_line () with
    | a ->
        if a = "A" || a = "a" then
          put_pieces_inplay player (prompt_piece_in_play player)
        else if a = "B" || a = "b" then update player (piece player) 6
        else failwith "invalid input"
  with
  | _ -> print_string "Err, you can't do that." |> fun _ -> six player

let rec roll_all x lst =
  if List.length lst > 0 then
    color_print_players
      ("\n\nPlayer "
      ^ string_of_int ((List.length lst - (x + 1)) * -1)
      ^ "\n")
      ((List.length lst - (x + 1)) * -1);
  match lst with
  | [] -> []
  | h :: t ->
      let num = roll 1 in
      if player_in_play h then
        if not (legal_moves_available h num) then
          print_string "\nYou can't make any valid moves\n" |> fun _ ->
          h :: roll_all x t
        else if num = 6 then
          let top = six h in
          top :: roll_all x t
        else
          let top = update h (piece h) num in
          top :: roll_all x t
      else if num = 6 then
        let top = put_pieces_inplay h (next_piece h) in
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
  Random.self_init ();
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
        if int_of_string players > 1 then
          play_game (int_of_string players)
        else raise NotEnoughPlayers
  with
  | NotEnoughPlayers ->
      print_endline "You need more players, find some friends!"
  | _ -> print_endline "Enter a valid number, please"

let () = main ()
