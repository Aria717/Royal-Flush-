(* @authors Sruthi Suresh (sls463), Advika Ravi Kumar (ar732), Aaradhyaa Gyawali
   (ag2223), Ishaan Nanal (isn6) *)
module Card = struct
  type suit =
    | Hearts
    | Diamonds
    | Clubs
    | Spades

  type rank =
    | Ace
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King

  type card = {
    rank : rank;
    suit : suit;
  }

  let hand_value (hand : card list) =
    let value_of_rank = function
      | Ace -> 11
      | Two -> 2
      | Three -> 3
      | Four -> 4
      | Five -> 5
      | Six -> 6
      | Seven -> 7
      | Eight -> 8
      | Nine -> 9
      | Ten | Jack | Queen | King -> 10
    in
    let rec sum_values hand =
      match hand with
      | [] -> 0
      | hd :: tl -> value_of_rank hd.rank + sum_values tl
    in
    let total = sum_values hand in
    if total > 21 then
      let rec adjust_for_aces hand total =
        match hand with
        | [] -> total
        | card :: tl ->
            if card.rank = Ace && total > 21 then adjust_for_aces tl (total - 10)
            else adjust_for_aces tl total
      in
      adjust_for_aces hand total
    else total

  let create_deck () =
    let ranks =
      [
        Ace;
        Two;
        Three;
        Four;
        Five;
        Six;
        Seven;
        Eight;
        Nine;
        Ten;
        Jack;
        Queen;
        King;
      ]
    in
    let suits = [ Hearts; Diamonds; Clubs; Spades ] in
    List.concat_map
      (fun suit -> List.map (fun rank -> { rank; suit }) ranks)
      suits

  let string_of_suit = function
    | Hearts -> "Hearts"
    | Diamonds -> "Diamonds"
    | Clubs -> "Clubs"
    | Spades -> "Spades"

  let string_of_rank = function
    | Ace -> "Ace"
    | Two -> "Two"
    | Three -> "Three"
    | Four -> "Four"
    | Five -> "Five"
    | Six -> "Six"
    | Seven -> "Seven"
    | Eight -> "Eight"
    | Nine -> "Nine"
    | Ten -> "Ten"
    | Jack -> "Jack"
    | Queen -> "Queen"
    | King -> "King"

  let print_card card =
    let rank_str =
      match card.rank with
      | Ace -> "A"
      | Two -> "2"
      | Three -> "3"
      | Four -> "4"
      | Five -> "5"
      | Six -> "6"
      | Seven -> "7"
      | Eight -> "8"
      | Nine -> "9"
      | Ten -> "10"
      | Jack -> "J"
      | Queen -> "Q"
      | King -> "K"
    in
    let suit_str =
      match card.suit with
      | Spades -> "\027[30m♠\027[0m" (* Black suits *)
      | Hearts -> "\027[31m♥\027[0m" (* Red suits *)
      | Diamonds -> "\027[31m♦\027[0m" (* Red suits *)
      | Clubs -> "\027[30m♣\027[0m" (* Black suits *)
    in
    let match_card =
      if card.rank = Ten then
        [
          "┌─────┐";
          "│" ^ rank_str ^ "   │";
          "│  " ^ suit_str ^ "  │";
          "│   " ^ rank_str ^ "│";
          "└─────┘";
        ]
      else
        [
          "┌─────┐";
          "│" ^ rank_str ^ "    │";
          "│  " ^ suit_str ^ "  │";
          "│    " ^ rank_str ^ "│";
          "└─────┘";
        ]
    in
    List.iter print_endline match_card

  let card_to_strings card =
    let rank_str =
      match card.rank with
      | Ace -> "A"
      | Two -> "2"
      | Three -> "3"
      | Four -> "4"
      | Five -> "5"
      | Six -> "6"
      | Seven -> "7"
      | Eight -> "8"
      | Nine -> "9"
      | Ten -> "10"
      | Jack -> "J"
      | Queen -> "Q"
      | King -> "K"
    in
    let suit_str =
      match card.suit with
      | Spades -> "\027[30m♠\027[0m" (* Black suits *)
      | Hearts -> "\027[31m♥\027[0m" (* Red suits *)
      | Diamonds -> "\027[31m♦\027[0m" (* Red suits *)
      | Clubs -> "\027[30m♣\027[0m" (* Black suits *)
    in
    if card.rank = Ten then
      [
        "┌─────┐";
        "│" ^ rank_str ^ "   │";
        "│  " ^ suit_str ^ "  │";
        "│   " ^ rank_str ^ "│";
        "└─────┘";
      ]
    else
      [
        "┌─────┐";
        "│" ^ rank_str ^ "    │";
        "│  " ^ suit_str ^ "  │";
        "│    " ^ rank_str ^ "│";
        "└─────┘";
      ]

  let print_card_list cards =
    let card_strings = List.map (fun x -> card_to_strings x) cards in
    let rec concat_lines acc = function
      | [] -> acc
      | h :: t ->
          let combined = List.map2 (fun x y -> x ^ " " ^ y) acc h in
          concat_lines combined t
    in
    match card_strings with
    | [] -> ()
    | first :: rest ->
        let combined_lines = concat_lines first rest in
        List.iter print_endline combined_lines

  let empty_card () : card list = []

  let string_of_card { rank; suit } =
    Printf.sprintf "%s of %s" (string_of_rank rank) (string_of_suit suit)

  let () = Random.self_init ()

  let rec pick_rand_card deck used =
    let num = Random.int (List.length deck) in
    let rand_card = List.nth deck num in
    if List.mem rand_card !used then pick_rand_card deck used else rand_card

  let rec deal_cards (deck : card list) used num_players count hand =
    if count > num_players then hand
    else
      let card1 = pick_rand_card deck used in
      used := card1 :: !used;
      let card2 = pick_rand_card deck used in
      used := card2 :: !used;
      let player_hand = [ card1; card2 ] in
      let updated_hand = player_hand :: hand in
      deal_cards deck used num_players (count + 1) updated_hand

  let rec card_to_player num_players cards count value =
    match cards with
    | [] -> print_endline ""
    | h :: t ->
        if count = value then (
          print_endline
            ("Player " ^ string_of_int count ^ "'s cards are: "
            ^ string_of_card (List.nth h 0));
          print_card (List.nth h 0))
        else (
          print_endline
            ("Player " ^ string_of_int count ^ "'s cards are: "
            ^ string_of_card (List.nth h 0)
            ^ " and "
            ^ string_of_card (List.nth h 1));
          let _ = print_card_list h in
          card_to_player (num_players - 1) t (count + 1) value)

  let total hand player_num =
    match List.nth_opt hand player_num with
    | Some player_hand -> hand_value player_hand
    | None -> failwith "Player's hand is empty."
end
