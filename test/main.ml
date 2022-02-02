open Stdlib
open Game
open Player_model
open OUnit2

let emptyplayer = []

let player0 = List.rev (create_pieces 4)

let player1 = List.rev (create_pieces 4) |> put_pieces_inplay 1

let player2 =
  List.rev (create_pieces 4)
  |> put_pieces_inplay 1 |> put_pieces_inplay 2

let player3 =
  List.rev (create_pieces 4)
  |> put_pieces_inplay 1 |> put_pieces_inplay 2 |> put_pieces_inplay 3

let player4 =
  List.rev (create_pieces 4)
  |> put_pieces_inplay 1 |> put_pieces_inplay 2 |> put_pieces_inplay 3
  |> put_pieces_inplay 4

let player5 = update_player player3 8 1

let winningplayer =
  update_player
    (update_player
       (update_player
          (update_player player4 Player_model.start 1)
          Player_model.start 2)
       Player_model.start 3)
    Player_model.start 4

let player_list_1 =
  [
    [
      { index = 1; position = 10; in_play = false };
      { index = 2; position = 10; in_play = false };
      { index = 3; position = 10; in_play = false };
      { index = 4; position = 10; in_play = false };
    ];
  ]

let player_model_tests =
  [
    ( "[create_pieces] test: 1 piece" >:: fun _ ->
      assert_equal
        [
          { index = 1; position = Player_model.start; in_play = false };
        ]
        (create_pieces 1) );
    ( "[create_pieces] test: 2 pieces" >:: fun _ ->
      assert_equal
        [
          { index = 2; position = Player_model.start; in_play = false };
          { index = 1; position = Player_model.start; in_play = false };
        ]
        (create_pieces 2) );
    ( "[create_pieces] test: 3 pieces" >:: fun _ ->
      assert_equal
        [
          { index = 3; position = Player_model.start; in_play = false };
          { index = 2; position = Player_model.start; in_play = false };
          { index = 1; position = Player_model.start; in_play = false };
        ]
        (create_pieces 3) );
    ( "[create_pieces] test: 4 pieces" >:: fun _ ->
      assert_equal
        [
          { index = 4; position = Player_model.start; in_play = false };
          { index = 3; position = Player_model.start; in_play = false };
          { index = 2; position = Player_model.start; in_play = false };
          { index = 1; position = Player_model.start; in_play = false };
        ]
        (create_pieces 4) );
    ( "[player_in_play] test: player with no active pieces is not in \
       play"
    >:: fun _ -> assert_equal false (player_in_play player0) );
    ( "[player_in_play] test: player with one active piece is in play"
    >:: fun _ -> assert_equal true (player_in_play player1) );
    ( "[player_in_play] test: player with two active pieces is in play"
    >:: fun _ -> assert_equal true (player_in_play player2) );
    ( "[player_in_play] test: player with three active pieces is in play"
    >:: fun _ -> assert_equal true (player_in_play player3) );
    ( "[player_in_play] test: player with four active pieces is in play"
    >:: fun _ -> assert_equal true (player_in_play player4) );
    ( "[score] test: score of player with no active pieces in play is 0"
    >:: fun _ -> assert_equal 0 (score player0) );
    ( "[score] test: score of player with four active and zero \
       finished pieces is 0"
    >:: fun _ -> assert_equal 0 (score player4) );
    ( "[score] test: score of player with four active pieces and one \
       finished piece is 1"
    >:: fun _ ->
      assert_equal 1
        (update_player player4 Player_model.start 1 |> score) );
    ( "[score] test: score of player with four active pieces and two \
       finished pieces is 1"
    >:: fun _ ->
      assert_equal 2
        (score
           (update_player
              (update_player player4 Player_model.start 1)
              Player_model.start 2)) );
    ( "[score] test: score of player with four active pieces and three \
       finished pieces is 3"
    >:: fun _ ->
      assert_equal 3
        (score
           (update_player
              (update_player
                 (update_player player4 Player_model.start 1)
                 Player_model.start 2)
              Player_model.start 3)) );
    ( "[score] test: score of player with four active pieces and four \
       finished pieces is 4"
    >:: fun _ -> assert_equal 4 (score winningplayer) );
    ( "[player_won] test: player with no active pieces has not won"
    >:: fun _ -> assert_equal false (player_won player0) );
    ( "[player_won] test: player with one active unfinished pieces has \
       not won"
    >:: fun _ -> assert_equal false (player_won player1) );
    ( "[player_won] test: player with four active unfinished pieces \
       has not won"
    >:: fun _ -> assert_equal false (player_won player4) );
    ( "[player_won] test: player with four active finished pieces has \
       won"
    >:: fun _ -> assert_equal true (player_won winningplayer) );
    ( "[put_pieces_inplay] test: player putting piece 1 in play"
    >:: fun _ -> assert_equal player1 (put_pieces_inplay 1 player0) );
    ( "[put_pieces_inplay] test: player putting piece 1 and 2 in play"
    >:: fun _ -> assert_equal player2 (put_pieces_inplay 2 player1) );
    ( "[put_pieces_inplay] test: player putting pieces 1,2,3 in play"
    >:: fun _ -> assert_equal player3 (put_pieces_inplay 3 player2) );
    ( "[put_pieces_inplay] test: player putting pieces 1,2,3,4 in play"
    >:: fun _ -> assert_equal player4 (put_pieces_inplay 4 player3) );
    ( "[update_player] test: player with piece 1 active rolls a 3 for \
       piece 1"
    >:: fun _ ->
      assert_equal
        [
          { index = 1; position = 9; in_play = true };
          { index = 2; position = 10; in_play = false };
          { index = 3; position = 10; in_play = false };
          { index = 4; position = 10; in_play = false };
        ]
        (update_player player1 1 1) );
    ( "[update_player] test: player with pieces 1,2 active and each \
       with position 2 rolls a 2 for piece 2"
    >:: fun _ ->
      assert_equal
        [
          { index = 1; position = 2; in_play = true };
          { index = 2; position = 0; in_play = true };
          { index = 3; position = 10; in_play = false };
          { index = 4; position = 10; in_play = false };
        ]
        (update_player
           [
             { index = 1; position = 2; in_play = true };
             { index = 2; position = 2; in_play = true };
             { index = 3; position = 10; in_play = false };
             { index = 4; position = 10; in_play = false };
           ]
           2 2) );
    ( "[pieces_in_play] test: no pieces in play" >:: fun _ ->
      assert_equal [] (pieces_in_play player0) );
    ( "[pieces_in_play] test: piece 1 in play" >:: fun _ ->
      assert_equal [ "1" ] (pieces_in_play player1) );
    ( "[pieces_in_play] test: all pieces in play" >:: fun _ ->
      assert_equal [ "1"; "2"; "3"; "4" ] (pieces_in_play player4) );
    ( "[pieces_not_in_play] test: no pieces not in play" >:: fun _ ->
      assert_equal [] (pieces_not_in_play player4) );
    ( "[pieces_not_in_play] test: piece 1 in play" >:: fun _ ->
      assert_equal [ "2"; "3"; "4" ] (pieces_not_in_play player1) );
    ( "[pieces_not_in_play] test: all pieces not in play" >:: fun _ ->
      assert_equal [] (pieces_not_in_play emptyplayer) );
    ( "[next_piece] test: player with no pieces in play has piece 1 \
       available as next piece"
    >:: fun _ -> assert_equal 1 (next_piece player0) );
    ( "[next_piece] test: player with piece 1 in play has piece 2 \
       available as next piece"
    >:: fun _ -> assert_equal 2 (next_piece player1) );
    ( "[next_piece] test: player with pieces 1,2 in play has piece 3 \
       available as next piece"
    >:: fun _ -> assert_equal 3 (next_piece player2) );
    ( "[next_piece] test: player with pieces 1,2,3 in play has piece 4 \
       available as next piece"
    >:: fun _ -> assert_equal 4 (next_piece player3) );
    ( "[next_piece] test: next piece of player with all pieces in play \
       raises [Failure]"
    >:: fun _ ->
      assert_raises (Failure "no more pieces") (fun () ->
          next_piece player4) );
    ( "[get_piece] test: getting piece from invalid piece index of \
       player with no pieces in play raises [Failure]"
    >:: fun _ ->
      assert_raises (Failure "not within indices 1-4") (fun () ->
          get_piece 5 player0) );
    ( "[get_piece] test: getting piece 1 of player with pieces 1 in play"
    >:: fun _ ->
      assert_equal
        { index = 1; position = 10; in_play = true }
        (get_piece 1 player1) );
    ( "[get_piece] test: getting piece 4 of player with pieces 1 in play"
    >:: fun _ ->
      assert_equal
        { index = 4; position = 10; in_play = false }
        (get_piece 4 player1) );
    ( "[winner] test: winner of game with no winning players raises \
       [Failure]"
    >:: fun _ ->
      assert_raises (Failure "nobody won") (fun () ->
          winner [ player0; player1; player2; player3 ]) );
    ( "[winner] test: winner of game with fourth player winning"
    >:: fun _ ->
      assert_equal winningplayer
        (winner [ player0; player1; player2; winningplayer ]) );
    ( "[create_players] test: creating a game with 4 players"
    >:: fun _ ->
      assert_equal
        [
          [
            { index = 1; position = 10; in_play = false };
            { index = 2; position = 10; in_play = false };
            { index = 3; position = 10; in_play = false };
            { index = 4; position = 10; in_play = false };
          ];
          [
            { index = 1; position = 10; in_play = false };
            { index = 2; position = 10; in_play = false };
            { index = 3; position = 10; in_play = false };
            { index = 4; position = 10; in_play = false };
          ];
          [
            { index = 1; position = 10; in_play = false };
            { index = 2; position = 10; in_play = false };
            { index = 3; position = 10; in_play = false };
            { index = 4; position = 10; in_play = false };
          ];
          [
            { index = 1; position = 10; in_play = false };
            { index = 2; position = 10; in_play = false };
            { index = 3; position = 10; in_play = false };
            { index = 4; position = 10; in_play = false };
          ];
        ]
        (create_players 4) );
    ( "[legal_move] test: piece at position 2 rolling a 3 is illegal"
    >:: fun _ ->
      assert_equal false
        (legal_move { index = 1; position = 2; in_play = true } 3) );
    ( "[legal_move] test: piece at position 4 rolling a 4 is legal"
    >:: fun _ ->
      assert_equal true
        (legal_move { index = 1; position = 4; in_play = true } 4) );
    ( "[legal_moves_available] test: player with no pieces in play \
       returns false"
    >:: fun _ -> assert_equal false (legal_moves_available player0 4) );
    ( "[legal_moves_available] test: player with only one piece in \
       play that is finished returns false"
    >:: fun _ ->
      assert_equal false
        (legal_moves_available (update_player player1 11 1) 4) );
    ( "[legal_moves_available] test: player with only two pieces in \
       play with one finished piece and an illegal roll for piece 2 \
       returns false"
    >:: fun _ ->
      assert_equal false
        (legal_moves_available
           (update_player (update_player player2 9 2) 10 1)
           4) );
    ( "[legal_moves_available] test: player with one pieces in play a \
       legal roll for piece 1 returns true"
    >:: fun _ ->
      assert_equal true
        (legal_moves_available (update_player player2 9 1) 1) );
    ( "game test: create_players" >:: fun _ ->
      assert_equal player_list_1 (create_players 1) );
    ( "game test: create_players" >:: fun _ ->
      assert_equal (player_list_1 @ player_list_1) (create_players 2) );
    ( "legal move test - false" >:: fun _ ->
      assert_equal false (legal_move (List.hd player0) 11) );
    ( "legal move test - true" >:: fun _ ->
      assert_equal true (legal_move (List.hd player0) 10) );
    ( "legal_moves_available test - true" >:: fun _ ->
      assert_equal true (legal_moves_available player4 10) );
    ( "legal_moves_available test - false" >:: fun _ ->
      assert_equal false (legal_moves_available player4 11) );
    ( "legal_moves_available test - false" >:: fun _ ->
      assert_equal false (legal_moves_available player0 10) );
    ( "legal_moves_available test - true" >:: fun _ ->
      assert_equal true (legal_moves_available player1 10) );
    ( "legal_moves_available test - true" >:: fun _ ->
      assert_equal true (legal_moves_available player5 10) );
    ( "[pieces_left] test: checking if player that has not that can be \
       moved won has pieces left"
    >:: fun _ -> assert_equal false (pieces_left winningplayer) );
    ( "[pieces_left] test: checking if player that has won if they \
       have pieces left"
    >:: fun _ -> assert_equal true (pieces_left player2) );
    ( "[pieces_left] test: checking if player1 that has not won has \
       pieces left"
    >:: fun _ -> assert_equal true (pieces_left player1) );
    ( "[pieces_left] test: checking if player4 that has not won has \
       pieces left.  It should not have pieces left because all pieces \
       are already in play"
    >:: fun _ -> assert_equal false (pieces_left player4) );
    ( "[pieces_left] test: checking if player5 that has not won has \
       pieces left.  It should not have pieces left because all pieces \
       are already in play"
    >:: fun _ -> assert_equal true (pieces_left player5) );
    ( "[pieces_able_to_play] test: checking if any players can move \
       move 1 place "
    >:: fun _ -> assert_equal [] (pieces_able_to_play winningplayer 1)
    );
    ( "[pieces_able_to_play] test: checking if any player1 can move 1 \
       piece "
    >:: fun _ ->
      assert_equal 1
        (List.length (pieces_able_to_play player1 3))
        ~printer:(fun x -> string_of_int x) );
    ( "[pieces_able_to_play] test: checking if any pieces can move 3 \
       spaces in player4 which should be all of them "
    >:: fun _ ->
      assert_equal player4 (pieces_able_to_play player4 3)
        ~printer:(fun x -> to_string_player x) );
    ( "[pieces_able_to_play] test: checking if any players can move 3 \
       spaces in player5 which should be all of them "
    >:: fun _ ->
      assert_equal 2
        (List.length (pieces_able_to_play player5 3))
        ~printer:(fun x -> string_of_int x) );
  ]

let game_tests = []

let suite =
  "test suite for Ludo"
  >::: List.flatten [ player_model_tests; game_tests ]

let _ = run_test_tt_main suite
