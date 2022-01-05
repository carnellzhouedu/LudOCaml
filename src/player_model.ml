let start = 57

type piece = {
  index : int;
  position : int;
  in_play : bool;
}

type player = piece list

let rec create_pieces p =
  if p = 1 then [ { index = p; position = start; in_play = false } ]
  else
    { index = p; position = start; in_play = false }
    :: create_pieces (p - 1)

let rec player_in_play = function
  | [] -> false
  | { in_play; position; _ } :: t ->
      (in_play && position > 0) || player_in_play t

let rec score = function
  | [] -> 0
  | { position; _ } :: t ->
      if position <= 0 then 1 + score t else score t

let player_won player = if score player = 4 then true else false

let put_piece_inplay piece =
  print_string
    ("\nYou now have piece " ^ string_of_int piece.index ^ " in play");
  { piece with in_play = true }

(* let take_piece_outplay piece = print_string ("Piece " ^ string_of_int
   piece.index ^ " has made it into the endzone"); { piece with won =
   false } *)

let rec put_pieces_inplay (player : player) piece_number =
  match player with
  | [] -> []
  | h :: t ->
      if h.index = piece_number then put_piece_inplay h :: t
      else h :: put_pieces_inplay t piece_number

(* let rec take_pieces_outplay (player : player) piece = match player
   with | [] -> [] | h :: t -> if h.index = piece then
   take_piece_outplay h :: t else h :: take_pieces_outplay t piece *)

let update_piece piece roll =
  print_string
    ("Piece "
    ^ string_of_int piece.index
    ^ " has "
    ^ string_of_int
        (if piece.position - roll < 0 then 0 else piece.position - roll)
    ^ " spaces left to win. "
    ^
    if piece.position - roll < 0 then
      "Your piece has made it to the endzone"
    else "");
  { piece with position = piece.position - roll }

(* let rec update_pieces player piece roll = match player with | [] ->
   [] | h :: t -> if h.index = piece then (update_piece h roll) :: t
   else h :: update_pieces t piece roll *)

let rec update_player player roll piece =
  match player with
  | [] -> []
  | h :: t ->
      if h.index = piece then update_piece h roll :: t
      else h :: update_player t roll piece

let rec piece_in_play_won player =
  match player with
  | [] -> false
  | h :: t -> (h.in_play && h.position <= 0) || piece_in_play_won t

let rec pieces_in_play = function
  | [] -> []
  | h :: t ->
      if h.in_play && h.position > 0 then
        string_of_int h.index :: pieces_in_play t
      else pieces_in_play t

let rec pieces_not_in_play = function
  | [] -> []
  | h :: t ->
      if (not h.in_play) && h.position > 0 then
        string_of_int h.index :: pieces_not_in_play t
      else pieces_not_in_play t

let rec next_piece = function
  | [] -> failwith "no more pieces"
  | h :: t ->
      if h.in_play = false && h.position > 0 then h.index
      else next_piece t

let rec get_piece n = function
  | [] -> failwith "not within indices 1-4"
  | h :: t -> if h.index = n then h else get_piece n t

let rec winner = function
  | [] -> failwith "nobody won"
  | h :: t -> if score h = 4 then h else winner t

let rec create_players p =
  if p = 1 then [ List.rev (create_pieces 4) ]
  else List.rev (create_pieces 4) :: create_players (p - 1)

let legal_move piece roll = piece.position - roll >= 0

let rec legal_moves_available player roll =
  match player with
  | [] -> false
  | h :: t ->
      (legal_move h roll && h.in_play) || legal_moves_available t roll
