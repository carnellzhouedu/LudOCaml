(** PLAYER INTERFACE *)

val start : int
(** [start] is the amount of spaces left once a piece becomes in play. *)

type piece = {
  index : int;
  position : int;
  in_play : bool;
}
(** [piece] represents the type of a piece *)

type player = piece list
(** [players] is a list containing the pieces for a player *)

val create_pieces : int -> player
(** [create_pieces n] initializes a player by creating a list of [n]
    pieces *)

val player_in_play : player -> bool
(** [player_in_play pl] returns whether player [pl] has any pieces in
    play *)

val score : player -> int
(** [score pl] returns the number of pieces that have made it to the
    endzone for player [pl] *)

val player_won : player -> bool
(** [player_won pl] returns whether all pieces of player [pl] have
    reached the endzoney and has won the game *)

val put_piece_inplay : piece -> piece
(** [put_piece_inplay pi] puts piece [pi] into play *)

val put_pieces_inplay : int -> player -> player
(** [put_pieces_inplay pl n] returns player [pl] with piece of piece
    number [n] put into play *)

(* val take_piece_outplay : piece -> piece

   val take_pieces_outplay : player -> int -> player *)

val update_piece : piece -> int -> piece
(** [update_piece pi n] returns the piece with its position updated by
    [n] steps *)

val update_player : player -> int -> int -> player
(** [update_player pl n pi] returns the player [pl] after moving piece
    pi [n] spaces *)

val pieces_not_in_play : player -> string list
(** [pieces_in_play pl] returns a list of pieces that are in not play
    for player [pl] *)

val pieces_in_play : player -> string list
(** [pieces_in_play pl] returns a list of pieces that are in play for
    player [pl] *)

val next_piece : player -> int
(** [next_piece pl] returns the index number of the next piece that is
    not yet in play for player [pl]. Raises [failwith "no more pieces"]
    if player [pl] has no pieces in play. *)

val get_piece : int -> player -> piece
(** [get_piece num pl] returns the piece based on its index number [num]
    for player [pl]. Raises [failwith "not within indices 1-4"] if
    player [pl] has no pieces in play. *)

(* val piece_in_play_won : player -> bool * [piece_in_play_won pl]
   returns the piece based on its index *)

val winner : player list -> player
(** [winner lst] returns the winning player from the list of players
    [lst] *)

val create_players : int -> piece list list
(** [create_players n] initializes a list of [n] participating players *)

val legal_move : piece -> int -> bool
(** [legal_move pi n] checks if piece [pi] of a player can be moved [n]
    spaces *)

val legal_moves_available : player -> int -> bool
(** [legal_moves_available pl n] checks if any pieces of player [pl] can
    be moved [n] spaces *)

val pieces_left : player -> bool
(** [pieces_left p] returns if the player has any more pieces left to
    move that are not currently play*)

val pieces_able_to_play : player -> int -> piece list
(** [piece_able_to_play pl roll] returns the pieces that can be moved
    given the current roll. EX if piece 1 is in play but only has two
    spots left then we they won't be returned.*)

val to_string_piece : piece -> string
(** [to_string pc] prints a pieces current position and whether or not
    it can be played*)

val to_string_player : player -> string
(** [to_string pl] prints all of the pieces of each player and each of
    the player's stats*)