(* @authors Sruthi Suresh (sls463), Advika Ravi Kumar (ar732), Aaradhyaa Gyawali
   (ag2223), Ishaan Nanal (isn6) *)
open OUnit2
open Blackjack
open Card
open Player

let test1 = Card.empty_card ()
let deck = Card.create_deck ()
let num_players = 4
let used = ref (Card.empty_card ())
let hands = ref (Card.deal_cards deck used num_players 1 [])
let player_wins = Array.make (num_players + 1) 0
let player_cash = Array.make (num_players + 1) 0
let busted = Array.make num_players 0
let player_sum1 = 10
let player_sum2 = 21
let player_sum3 = 50
let hand_list = Card.deal_cards deck used 2 1 []
let hand = List.nth hand_list 1
let new_hand_1 = Player.hit hand deck used
let deck_tests = Card.deal_cards deck used 1 1 []
let test_rank_Two = Card.Two
let test_rank_Three = Card.Three
let test_rank_Four = Card.Four
let test_rank_Five = Card.Five
let test_rank_Six = Card.Six
let test_rank_Seven = Card.Seven
let test_rank_Eight = Card.Eight
let test_rank_Nine = Card.Nine
let test_rank_Ten = Card.Ten
let test_rank_Jack = Card.Jack
let test_rank_Queen = Card.Queen
let test_rank_King = Card.King
let test_rank_Ace = Card.Ace
let test_suit_Hearts = Card.Hearts
let test_suit_Clubs = Card.Clubs
let test_suit_Diamonds = Card.Diamonds
let test_suit_Spades = Card.Spades
let hand = [ { Card.rank = Ace; suit = Hearts } ]
let card1 = { Card.rank = Ace; suit = Hearts }
let card2 = { Card.rank = Two; suit = Diamonds }
let card3 = { Card.rank = Three; suit = Clubs }
let card4 = { Card.rank = Four; suit = Spades }
let card5 = { Card.rank = Five; suit = Hearts }
let card6 = { Card.rank = Six; suit = Diamonds }
let card7 = { Card.rank = Seven; suit = Clubs }
let card8 = { Card.rank = Eight; suit = Spades }
let card9 = { Card.rank = Nine; suit = Hearts }
let card10 = { Card.rank = Ten; suit = Diamonds }
let card11 = { Card.rank = Jack; suit = Clubs }
let card12 = { Card.rank = Queen; suit = Spades }
let card13 = { Card.rank = King; suit = Hearts }
let card14 = { Card.rank = Ace; suit = Diamonds }
let card15 = { Card.rank = Two; suit = Clubs }

let hand1 =
  [ { Card.rank = Ace; suit = Hearts }; { rank = King; suit = Spades } ]

let hand2 =
  [ { Card.rank = Two; suit = Hearts }; { rank = King; suit = Hearts } ]

let hand3 =
  [ { Card.rank = Three; suit = Clubs }; { rank = Ten; suit = Diamonds } ]

let hand4 =
  [ { Card.rank = Four; suit = Spades }; { rank = Nine; suit = Hearts } ]

let hand5 =
  [ { Card.rank = Five; suit = Hearts }; { rank = Eight; suit = Spades } ]

let hand6 =
  [ { Card.rank = Six; suit = Diamonds }; { rank = Seven; suit = Clubs } ]

let hand7 =
  [ { Card.rank = Seven; suit = Clubs }; { rank = Six; suit = Diamonds } ]

let hand8 =
  [ { Card.rank = Eight; suit = Spades }; { rank = Five; suit = Hearts } ]

let hand9 =
  [ { Card.rank = Nine; suit = Hearts }; { rank = Four; suit = Spades } ]

let hand10 =
  [ { Card.rank = Ten; suit = Diamonds }; { rank = Three; suit = Clubs } ]

let hand11 =
  [ { Card.rank = Jack; suit = Clubs }; { rank = Two; suit = Diamonds } ]

let hand12 =
  [ { Card.rank = Queen; suit = Spades }; { rank = Ace; suit = Hearts } ]

let hand13 =
  [ { Card.rank = King; suit = Hearts }; { rank = King; suit = Spades } ]

let hand14 =
  [ { Card.rank = Ace; suit = Diamonds }; { rank = Ace; suit = Hearts } ]

let hand15 =
  [ { Card.rank = Two; suit = Clubs }; { rank = Queen; suit = Spades } ]

let hand16 =
  [ { Card.rank = King; suit = Spades }; { rank = Queen; suit = Spades } ]

let list_to_test_total = [ hand1; hand2 ]
let picked_card = Card.pick_rand_card hand1 used
let list_to_test_total1 = [ hand1; hand2 ]
let list_to_test_total2 = [ hand3; hand4 ]
let list_to_test_total3 = [ hand5; hand6 ]
let list_to_test_total4 = [ hand7; hand8 ]
let list_to_test_total5 = [ hand9; hand10 ]
let list_to_test_total6 = [ hand11; hand12 ]
let list_to_test_total7 = [ hand13; hand14 ]
let list_to_test_total8 = [ hand15; hand1 ]
let list_to_test_total9 = [ hand2; hand3 ]
let list_to_test_total10 = [ hand4; hand5 ]
let list_to_test_total11 = [ hand6; hand7 ]
let list_to_test_total12 = [ hand8; hand9 ]
let list_to_test_total13 = [ hand10; hand11 ]
let list_to_test_total14 = [ hand12; hand13 ]
let list_to_test_total15 = [ hand14; hand15 ]
let picked_card1 = Card.pick_rand_card hand1 used
let picked_card2 = Card.pick_rand_card hand2 used
let picked_card3 = Card.pick_rand_card hand3 used
let picked_card4 = Card.pick_rand_card hand4 used
let picked_card5 = Card.pick_rand_card hand5 used
let picked_card6 = Card.pick_rand_card hand6 used
let picked_card7 = Card.pick_rand_card hand7 used
let picked_card8 = Card.pick_rand_card hand8 used
let picked_card9 = Card.pick_rand_card hand9 used
let picked_card10 = Card.pick_rand_card hand10 used
let picked_card11 = Card.pick_rand_card hand11 used
let picked_card12 = Card.pick_rand_card hand12 used
let picked_card13 = Card.pick_rand_card hand13 used
let picked_card14 = Card.pick_rand_card hand14 used
let picked_card15 = Card.pick_rand_card hand15 used
let hand_list = Card.deal_cards deck used 2 1 []
let hand_list1 = Card.deal_cards deck used 2 1 []
let hand_list2 = Card.deal_cards deck used 3 1 []

let tests =
  [
    ("empty" >:: fun _ -> assert_equal 0 (List.length test1));
    ("check_hand_length" >:: fun _ -> assert_equal 4 (List.length !hands));
    ( "check_hand_length1" >:: fun _ ->
      assert_equal 2 (List.length (List.nth !hands 0)) );
    ( "check_hand_length2" >:: fun _ ->
      assert_equal 2 (List.length (List.nth !hands 1)) );
    ( "check_hand_length3" >:: fun _ ->
      assert_equal 2 (List.length (List.nth !hands 2)) );
    ( "check_hand_length4" >:: fun _ ->
      assert_equal 2 (List.length (List.nth !hands 3)) );
    ("check_pw_length" >:: fun _ -> assert_equal 5 (Array.length player_wins));
    ("check_pc_length" >:: fun _ -> assert_equal 5 (Array.length player_cash));
    ("check_busted_length" >:: fun _ -> assert_equal 4 (Array.length busted));
    ( "extract" >:: fun _ ->
      assert_equal (Player.extract hand_list 0) (List.nth hand_list 0) );
    ( "extract2" >:: fun _ ->
      assert_equal (Player.extract hand_list 1) (List.nth hand_list 1) );
    ("hit" >:: fun _ -> assert (List.length new_hand_1 > 1));
    ( "hit1" >:: fun _ ->
      let hand = [ { Card.rank = Seven; suit = Diamonds } ] in
      let new_hand = Player.hit hand deck used in
      assert (List.length new_hand > List.length hand) );
    ( "hit3" >:: fun _ ->
      let hand = [ { Card.rank = Nine; suit = Spades } ] in
      let new_hand = Player.hit hand deck used in
      assert (List.length new_hand > List.length hand) );
    ( "hit4" >:: fun _ ->
      let hand = [ { Card.rank = Ten; suit = Hearts } ] in
      let new_hand = Player.hit hand deck used in
      assert (List.length new_hand > List.length hand) );
    ( "hit6" >:: fun _ ->
      let hand = [ { Card.rank = Queen; suit = Clubs } ] in
      let new_hand = Player.hit hand deck used in
      assert (List.length new_hand > List.length hand) );
    ( "hit7" >:: fun _ ->
      let hand = [ { Card.rank = King; suit = Spades } ] in
      let new_hand = Player.hit hand deck used in
      assert (List.length new_hand > List.length hand) );
    ( "hit10" >:: fun _ ->
      let hand = [ { Card.rank = Ten; suit = Hearts } ] in
      let new_hand = Player.hit hand deck used in
      assert (List.length new_hand > List.length hand) );
    (* ( "hit11" >:: fun _ -> let hand = [ { Card.rank = Jack; suit = Diamonds }
       ] in let new_hand = Player.hit hand deck used in assert (List.length
       new_hand > List.length hand) ); *)
    ( "hit12" >:: fun _ ->
      let hand = [ { Card.rank = Queen; suit = Clubs } ] in
      let new_hand = Player.hit hand deck used in
      assert (List.length new_hand > List.length hand) );
    (* ( "hit13" >:: fun _ -> let hand = [ { Card.rank = King; suit = Spades } ]
       in let new_hand = Player.hit hand deck used in assert (List.length
       new_hand > List.length hand) ); ( "hit14" >:: fun _ -> let hand = [ {
       Card.rank = Two; suit = Diamonds } ] in let new_hand = Player.hit hand
       deck used in assert (List.length new_hand > List.length hand) ); (
       "hit15" >:: fun _ -> let hand = [ { Card.rank = Three; suit = Hearts } ]
       in let new_hand = Player.hit hand deck used in assert (List.length
       new_hand > List.length hand) );*)
    ( "hit16" >:: fun _ ->
      let hand = [ { Card.rank = Four; suit = Clubs } ] in
      let new_hand = Player.hit hand deck used in
      assert (List.length new_hand > List.length hand) );
    ("bust" >:: fun _ -> assert_equal false (Player.bust player_sum1));
    ("bust1" >:: fun _ -> assert_equal false (Player.bust player_sum2));
    ("bust2" >:: fun _ -> assert_equal true (Player.bust player_sum3));
    ("empty" >:: fun _ -> assert_equal 0 (List.length test1));
    ("Two" >:: fun _ -> assert_equal "Two" (Card.string_of_rank test_rank_Two));
    ( "Three" >:: fun _ ->
      assert_equal "Three" (Card.string_of_rank test_rank_Three) );
    ( "Four" >:: fun _ ->
      assert_equal "Four" (Card.string_of_rank test_rank_Four) );
    ( "Five" >:: fun _ ->
      assert_equal "Five" (Card.string_of_rank test_rank_Five) );
    ("Six" >:: fun _ -> assert_equal "Six" (Card.string_of_rank test_rank_Six));
    ( "Seven" >:: fun _ ->
      assert_equal "Seven" (Card.string_of_rank test_rank_Seven) );
    ( "Eight" >:: fun _ ->
      assert_equal "Eight" (Card.string_of_rank test_rank_Eight) );
    ( "Nine" >:: fun _ ->
      assert_equal "Nine" (Card.string_of_rank test_rank_Nine) );
    ("Ten" >:: fun _ -> assert_equal "Ten" (Card.string_of_rank test_rank_Ten));
    ( "Jack" >:: fun _ ->
      assert_equal "Jack" (Card.string_of_rank test_rank_Jack) );
    ( "Jack" >:: fun _ ->
      assert_equal "Jack" (Card.string_of_rank test_rank_Jack) );
    ( "Queen" >:: fun _ ->
      assert_equal "Queen" (Card.string_of_rank test_rank_Queen) );
    ( "King" >:: fun _ ->
      assert_equal "King" (Card.string_of_rank test_rank_King) );
    ("Ace" >:: fun _ -> assert_equal "Ace" (Card.string_of_rank test_rank_Ace));
    ( "Hearts" >:: fun _ ->
      assert_equal "Hearts" (Card.string_of_suit test_suit_Hearts) );
    ( "Spades" >:: fun _ ->
      assert_equal "Spades" (Card.string_of_suit test_suit_Spades) );
    ( "Diamonds" >:: fun _ ->
      assert_equal "Diamonds" (Card.string_of_suit test_suit_Diamonds) );
    ( "Clubs" >:: fun _ ->
      assert_equal "Clubs" (Card.string_of_suit test_suit_Clubs) );
    ("deck" >:: fun _ -> assert_equal 52 (List.length deck));
    ("hand_value" >:: fun _ -> assert_equal 21 (Card.hand_value hand1));
    ("hand_value2" >:: fun _ -> assert_equal 12 (Card.hand_value hand2));
    ("hand value 3" >:: fun _ -> assert_equal 13 (Card.hand_value hand3));
    ("hand value 4" >:: fun _ -> assert_equal 13 (Card.hand_value hand4));
    ("hand value 5" >:: fun _ -> assert_equal 13 (Card.hand_value hand5));
    ("hand value 6" >:: fun _ -> assert_equal 13 (Card.hand_value hand6));
    ("hand value 7" >:: fun _ -> assert_equal 13 (Card.hand_value hand7));
    ("hand value 8" >:: fun _ -> assert_equal 13 (Card.hand_value hand8));
    ("hand value 9" >:: fun _ -> assert_equal 13 (Card.hand_value hand9));
    ("hand value 10" >:: fun _ -> assert_equal 13 (Card.hand_value hand10));
    ("hand value 11" >:: fun _ -> assert_equal 12 (Card.hand_value hand11));
    ("hand value 12" >:: fun _ -> assert_equal 21 (Card.hand_value hand12));
    ("hand value 13" >:: fun _ -> assert_equal 20 (Card.hand_value hand13));
    ("hand value 14" >:: fun _ -> assert_equal 12 (Card.hand_value hand14));
    ("hand value 15" >:: fun _ -> assert_equal 12 (Card.hand_value hand15));
    ( "string of card 1" >:: fun _ ->
      assert_equal "Ace of Hearts" (Card.string_of_card card1) );
    ( "string of card 2" >:: fun _ ->
      assert_equal "Two of Diamonds" (Card.string_of_card card2) );
    ( "string of card 3" >:: fun _ ->
      assert_equal "Three of Clubs" (Card.string_of_card card3) );
    ( "string of card 4" >:: fun _ ->
      assert_equal "Four of Spades" (Card.string_of_card card4) );
    ( "string of card 5" >:: fun _ ->
      assert_equal "Five of Hearts" (Card.string_of_card card5) );
    ( "string of card 6" >:: fun _ ->
      assert_equal "Six of Diamonds" (Card.string_of_card card6) );
    ( "string of card 7" >:: fun _ ->
      assert_equal "Seven of Clubs" (Card.string_of_card card7) );
    ( "string of card 8" >:: fun _ ->
      assert_equal "Eight of Spades" (Card.string_of_card card8) );
    ( "string of card 9" >:: fun _ ->
      assert_equal "Nine of Hearts" (Card.string_of_card card9) );
    ( "string of card 10" >:: fun _ ->
      assert_equal "Ten of Diamonds" (Card.string_of_card card10) );
    ( "string of card 11" >:: fun _ ->
      assert_equal "Jack of Clubs" (Card.string_of_card card11) );
    ( "string of card 12" >:: fun _ ->
      assert_equal "Queen of Spades" (Card.string_of_card card12) );
    ( "string of card 13" >:: fun _ ->
      assert_equal "King of Hearts" (Card.string_of_card card13) );
    ( "string of card 14" >:: fun _ ->
      assert_equal "Ace of Diamonds" (Card.string_of_card card14) );
    ( "string of card 15" >:: fun _ ->
      assert_equal "Two of Clubs" (Card.string_of_card card15) );
    ("pick random card" >:: fun _ -> assert (List.mem picked_card hand1));
    ("total" >:: fun _ -> assert_equal (Card.total list_to_test_total 0) 21);
    ("total2" >:: fun _ -> assert_equal (Card.total list_to_test_total 1) 12);
    ("total" >:: fun _ -> assert_equal (Card.total list_to_test_total1 0) 21);
    ("total2" >:: fun _ -> assert_equal (Card.total list_to_test_total1 1) 12);
    ("total 1" >:: fun _ -> assert_equal (Card.total list_to_test_total1 0) 21);
    ("total 2" >:: fun _ -> assert_equal (Card.total list_to_test_total2 0) 13);
    ("total 3" >:: fun _ -> assert_equal (Card.total list_to_test_total3 0) 13);
    ("total 4" >:: fun _ -> assert_equal (Card.total list_to_test_total4 0) 13);
    ("total 5" >:: fun _ -> assert_equal (Card.total list_to_test_total5 0) 13);
    ("total 6" >:: fun _ -> assert_equal (Card.total list_to_test_total6 0) 12);
    ("total 7" >:: fun _ -> assert_equal (Card.total list_to_test_total7 0) 20);
    ("total 8" >:: fun _ -> assert_equal (Card.total list_to_test_total8 0) 12);
    ("total 9" >:: fun _ -> assert_equal (Card.total list_to_test_total9 0) 12);
    ("total 10" >:: fun _ -> assert_equal (Card.total list_to_test_total10 0) 13);
    ("total 11" >:: fun _ -> assert_equal (Card.total list_to_test_total11 0) 13);
    ("total 12" >:: fun _ -> assert_equal (Card.total list_to_test_total12 0) 13);
    ("total 13" >:: fun _ -> assert_equal (Card.total list_to_test_total13 0) 13);
    ("total 14" >:: fun _ -> assert_equal (Card.total list_to_test_total14 0) 21);
    ("total 15" >:: fun _ -> assert_equal (Card.total list_to_test_total15 0) 12);
    ("pick random card 1" >:: fun _ -> assert (List.mem picked_card1 hand1));
    ("pick random card 2" >:: fun _ -> assert (List.mem picked_card2 hand2));
    ("pick random card 3" >:: fun _ -> assert (List.mem picked_card3 hand3));
    ("pick random card 4" >:: fun _ -> assert (List.mem picked_card4 hand4));
    ("pick random card 5" >:: fun _ -> assert (List.mem picked_card5 hand5));
    ("pick random card 6" >:: fun _ -> assert (List.mem picked_card6 hand6));
    ("pick random card 7" >:: fun _ -> assert (List.mem picked_card7 hand7));
    ("pick random card 8" >:: fun _ -> assert (List.mem picked_card8 hand8));
    ("pick random card 9" >:: fun _ -> assert (List.mem picked_card9 hand9));
    ("pick random card 10" >:: fun _ -> assert (List.mem picked_card10 hand10));
    ("pick random card 11" >:: fun _ -> assert (List.mem picked_card11 hand11));
    ("pick random card 12" >:: fun _ -> assert (List.mem picked_card12 hand12));
    ("pick random card 13" >:: fun _ -> assert (List.mem picked_card13 hand13));
    ("pick random card 14" >:: fun _ -> assert (List.mem picked_card14 hand14));
    ("pick random card 15" >:: fun _ -> assert (List.mem picked_card15 hand15));
    ( "extract" >:: fun _ ->
      assert_equal (Player.extract hand_list 0) (List.nth hand_list 0) );
    ( "extract2" >:: fun _ ->
      assert_equal (Player.extract hand_list 1) (List.nth hand_list 1) );
    ( "extract3" >:: fun _ ->
      assert_equal (Player.extract hand_list1 0) (List.nth hand_list1 0) );
    ( "extract4" >:: fun _ ->
      assert_equal (Player.extract hand_list1 1) (List.nth hand_list1 1) );
    ( "extract5" >:: fun _ ->
      assert_equal (Player.extract hand_list2 0) (List.nth hand_list2 0) );
    ( "extract6" >:: fun _ ->
      assert_equal (Player.extract hand_list2 1) (List.nth hand_list2 1) );
    ( "extract7" >:: fun _ ->
      assert_equal (Player.extract hand_list2 2) (List.nth hand_list2 2) );
    ( "extract8" >:: fun _ ->
      let hand_list3 =
        [
          [ { Card.rank = Six; suit = Diamonds } ];
          [ { Card.rank = Seven; suit = Clubs } ];
          [ { Card.rank = Eight; suit = Spades } ];
          [ { Card.rank = Nine; suit = Hearts } ];
        ]
      in
      assert_equal (Player.extract hand_list3 0) (List.nth hand_list3 0) );
    ( "extract9" >:: fun _ ->
      let hand_list3 =
        [
          [ { Card.rank = Six; suit = Diamonds } ];
          [ { Card.rank = Seven; suit = Clubs } ];
          [ { Card.rank = Eight; suit = Spades } ];
          [ { Card.rank = Nine; suit = Hearts } ];
        ]
      in
      assert_equal (Player.extract hand_list3 1) (List.nth hand_list3 1) );
    ( "extract10" >:: fun _ ->
      let hand_list3 =
        [
          [ { Card.rank = Six; suit = Diamonds } ];
          [ { Card.rank = Seven; suit = Clubs } ];
          [ { Card.rank = Eight; suit = Spades } ];
          [ { Card.rank = Nine; suit = Hearts } ];
        ]
      in
      assert_equal (Player.extract hand_list3 2) (List.nth hand_list3 2) );
    ( "extract11" >:: fun _ ->
      let hand_list3 =
        [
          [ { Card.rank = Six; suit = Diamonds } ];
          [ { Card.rank = Seven; suit = Clubs } ];
          [ { Card.rank = Eight; suit = Spades } ];
          [ { Card.rank = Nine; suit = Hearts } ];
        ]
      in
      assert_equal (Player.extract hand_list3 3) (List.nth hand_list3 3) );
  ]

let test_suite = "Blackjack Test " >::: tests
let _ = run_test_tt_main test_suite
