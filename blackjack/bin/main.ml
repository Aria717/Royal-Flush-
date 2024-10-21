(* @authors Sruthi Suresh (sls463), Advika Ravi Kumar (ar732), Aaradhyaa Gyawali
   (ag2223), Ishaan Nanal (isn6) *)
open Blackjack.Card
open Blackjack.Player

(* Done: fix sum, choosing winner, busting, dealer backend, printing
   instructions*)
(* To Implement: test cases*)

(** Takes the number of players and prints out their names *)

let print_blackjack_title () =
  let title = [
    "";
    "██████╗ ██╗      █████╗  ██████╗██╗  ██╗   ██████╗  █████╗  ██████╗██╗  ██╗";
    "██╔══██╗██║     ██╔══██╗██╔════╝██║ ██╔╝      ██╔╝ ██╔══██╗██╔════╝██║ ██╔╝";
    "██████╔╝██║     ███████║██║     █████╔╝       ██║  ███████║██║     █████╔╝ ";
    "██╔══██╗██║     ██╔══██║██║     ██╔═██╗  ██╗  ██║  ██╔══██║██║     ██╔═██╗ ";
    "██████╔╝███████╗██║  ██║╚██████╗██║  ██╗ ███████║  ██║  ██║╚██████╗██║  ██║ ";
    "╚═════╝ ╚══════╝╚═╝  ╚═╝ ╚═════╝╚═╝  ╚═╝ ╚══════╝  ╚═╝  ╚═╝ ╚═════╝╚═╝  ╚═╝ ";
    "                                       ██╗ "; 
    "                                       ╚═╝ ";
    ""
  ] in
  List.iter print_endline title

let () = print_blackjack_title()

  let print_jackblack_title () =
    let title = [
      "";
      "  ██████╗  █████╗  ██████╗██╗  ██╗ ██████╗ ██╗      █████╗  ██████╗██╗  ██╗  ";
      "     ██╔╝ ██╔══██╗██╔════╝██║ ██╔╝ ██╔══██╗██║     ██╔══██╗██╔════╝██║ ██╔╝ ";
      "     ██║  ███████║██║     █████╔╝  ██████╔╝██║     ███████║██║     █████╔╝  ";
      "██╗  ██║  ██╔══██║██║     ██╔═██╗  ██╔══██╗██║     ██╔══██║██║     ██╔═██╗ ";
      "███████║  ██║  ██║╚██████╗██║  ██║ ██████╔╝███████╗██║  ██║╚██████╗██║  ██╗";
      "╚══════╝  ╚═╝  ╚═╝ ╚═════╝╚═╝  ╚═╝ ╚═════╝ ╚══════╝╚═╝  ╚═╝ ╚═════╝╚═╝  ╚═╝ ";
      "                                 ██╗ "; 
      "                                 ╚═╝ ";
      ""
    ] in
    List.iter print_endline title

(** Takes the number of players and prints out their names *)
let rec get_player_names num_players count =
  match num_players - 1 with
  | 0 -> print_endline ""
  | 1 ->
      let () =
        print_endline "";
        print_endline "Please enter Player name"
      in
      let player_name = read_line () in
      print_endline ("Player " ^ string_of_int count ^ ": " ^ player_name);
      get_player_names (num_players - 1) (count + 1)
  | 2 ->
      let () =
        print_endline "";
        print_endline "Please enter Player name"
      in
      let player_name = read_line () in
      print_endline ("Player " ^ string_of_int count ^ ": " ^ player_name);
      get_player_names (num_players - 1) (count + 1)
  | 3 ->
      let () =
        print_endline "";
        print_endline "Please enter Player name"
      in
      let player_name = read_line () in
      print_endline ("Player " ^ string_of_int count ^ ": " ^ player_name);
      get_player_names (num_players - 1) (count + 1)
  | 4 ->
      let () =
        print_endline "";
        print_endline "Please enter Player name"
      in
      let player_name = read_line () in
      print_endline ("Player " ^ string_of_int count ^ ": " ^ player_name);
      get_player_names (num_players - 1) (count + 1)
  | _ -> print_endline ""

let print_start () =
  print_endline "Are you ready to lose some money !?!?";
  print_endline "We’d like to welcome everybody to the 8th Wonder of the world.";
  print_endline
    "Aaradhya, Sruthi, Advika, and Ishaan proudly present the event of the \
     century, the always timeless game where WE make YOUR luck 😉.";
  print_endline
    "You could’ve been anywhere in the world tonight, but you’re here with us… \
     we appreciate that.";
  print_endline "";
  print_endline "Below are the instructions for our wonderful game:";
  print_endline "Proceed with caution and may the luckiest person win!";
  print_endline ""

let () = print_start ()

let rec print_instruc () =
  let () = print_endline "" in
  let () = print_endline "" in
  let () =
    print_endline
      "Would you like to read instructions for the game of BlackJack? \n\
       Please enter 'y' or 'n'."
  in
  let the_input = read_line () in
  match String.lowercase_ascii the_input with
  | "y" ->
      print_endline
        "You will be asked if you would like to play a game of blackjack";
      print_endline
        "Type either ‘y’ or ‘n’ (‘y’ meaning yes and ‘n’ meaning no) to \
         confirm or deny your participation";
      print_endline
        "You will then be asked how many people will be joining the game";
      print_endline "You may type any number between one and four";
      print_endline
        "BUT… there is another secret player that you will not need to input";
      print_endline
        "The dealer!! In Blackjack you’re not actually playing against other \
         players, you’re only opponent is the dealer";
      print_endline
        "Regardless of what the other players get: the object of the game is \
         to beat the dealer (in this case the computer)";
      print_endline "";
      print_endline "Before starting the game you will input each player’s name";
      print_endline
        "At the start of the game, each player will receive two cards";
      print_endline
        "Don’t worry about being discrete, in this game everybody can look at \
         each other’s cards";
      print_endline
        "The cards will be printed in the terminal along with each player’s \
         name so you know which cards belong to which player";
      print_endline "";
      print_endline "Rules of the game:";
      print_endline
        "The end goal of the game is to get as close to 21 points as possible \
         without going over";
      print_endline
        "If you go over 21 you have “busted” and are out of the game";
      print_endline "Each card you are dealt has a point value assigned to it";
      print_endline "Number cards are worth the amount written on them";
      print_endline "All face cards (Jack, Queen, and King) are worth ten";
      print_endline "The sum of the cards that you are dealt is your score";
      print_endline "Ace is a special card";
      print_endline
        "It is worth eleven points, however, if you bust, the value resets to \
         one";
      print_endline "";
      print_endline "Once your turn begins you will have two options";
      print_endline "You can either hit or stand";
      print_endline
        "If you choose to stand then your turn ends and the next player’s turn \
         will begin";
      print_endline "If you choose to hit then you will be dealt another card";
      print_endline
        "You will have the option to continue to “hit” until either one of two \
         scenarios occurs";
      print_endline "Either you “bust” and your score will be greater than 21";
      print_endline "Or you choose to stand";
      print_endline "";
      print_endline
        "Each player will get a turn and once all players have finished the \
         computer (dealer) will begin its turn";
      print_endline
        "If the computer has a better score (closer to 21) than any player \
         then it wins and everyone else loses";
      print_endline
        "If it doesn’t, then the person with the best score will win!! If \
         multiple people have the same score then they will tie and split the \
         reward";
      print_endline "";
      print_endline "If you score exactly 21 points then you have a Blackjack!!";
      print_endline "And your reward will double";
      print_endline "";
      print_endline
        "We hope this helped get you excited about participating in this \
         tournament of champions";
      print_endline "This conquest of heroes";
      print_endline "This roundtable of excellence";
      print_endline "They say winners aren’t born… they’re made";
      print_endline
        "Well, today is the day for you to make your legacy and stamp it \
         forever in the ASAI record books";
      print_endline "";
      print_endline "Good luck :))";
      print_endline "May the odds be ever in your favor!!";
      print_endline ""
  | "n" -> print_endline "Okay, someone's an expert! Moving on then."
  | _ ->
      print_endline "Invalid response, please type either 'y' or 'n'.";
      print_instruc ()

let () = print_instruc ()

let rec prompt_and_print () =
  let () = print_endline "" in
  let () = print_endline "" in
  let () = print_endline "Would you like to start a game of BlackJack?" in
  let the_input = read_line () in
  match String.lowercase_ascii the_input with
  | "y" ->
      print_endline "Good luck!";
      let () =
        print_endline "";
        print_endline "Would you like instructions for betting?";
        let the_input = read_line () in
        match String.lowercase_ascii the_input with
        | "y" ->
            print_endline "";
            print_endline "So you wanna bet money huh?";
            print_endline "Be our guest!!";
            print_endline "";
            print_endline
              "Gambling in our game of blackjack works a bit differently than \
               you may be accustomed to. You don’t have a sum of money \
               pre-allotted to you that could potentially run out over the \
               course of the rounds. Instead, you have the option of playing a \
               new round or walking away at the end of every round.";
            print_endline "";
            print_endline
              "Each round costs $10 to enter per person. The closest person to \
               21 wins the round. At the end of each round, the winners get \
               1.5x their original bet (so if you win you will get $15). \
               Unless you win with a blackjack, in which case you get 2x your \
               original bet ($20). If you don’t win, your bet goes to the \
               dealer (the house).";
            print_endline "";
            print_endline
              "For example, if there is a 4-player game (5 including the \
               dealer) and players 1 and 2 both win with 20, then both players \
               1 and 2 will have $15 at the end of the round. Players 3 and 4 \
               will have $0 since they bet $10 and couldn’t win it back. The \
               dealer will get the bets of the losing players (3 and 4) and \
               will have $20 at the end of the round.";
            print_endline "";
            print_endline
              "Pssst: This ladies and gentlemen is how the 'house always wins' \
               😉"
        | "n" -> print_endline "Great! We count on irresponsible gamblers ;)"
        | _ ->
            print_endline "";
            print_endline "Invalid response, please type either 'y' or 'n'.";
            prompt_and_print ()
      in
      ()
  | "n" ->
      print_endline "Goodbye!";
      exit 0
  | "jackblack" -> print_endline "";
        print_endline "you found our little easter egg '🥚'";
        print_endline "";
        print_endline "The Dragon Warrior Has Come!";
        print_endline "";
        print_jackblack_title ();
        print_endline "";
        prompt_and_print ();
  | _ ->
      print_endline "Invalid response, please type either 'y' or 'n'.";
      prompt_and_print ()

let () = prompt_and_print ()

let rec get_num_players () =
  print_endline "";
  let () = print_endline "How many players will be playing?" in
  let input = read_line () in
  match int_of_string_opt input with
  | Some num ->
      if num >= 1 && num <= 4 then num
      else (
        print_endline "Invalid input. Please enter a number between 1 and 4.";
        get_num_players ())
  | None ->
      print_endline "Invalid input. Please enter a valid integer.";
      get_num_players ()

let clear_terminal () = print_string "\027[2J\027[H"
let num_players = get_num_players () + 1

(* Define a mutable array to store the number of wins for each player *)
let player_wins = Array.make (num_players + 1) 0
let player_cash = Array.make (num_players + 1) 0
let busted = Array.make num_players 0

let update_cash cash (bljk : bool) =
  if bljk then
    List.iter
      (fun (cash, _) -> player_cash.(cash) <- player_cash.(cash) + 20)
      cash
  else
    List.iter
      (fun (cash, _) -> player_cash.(cash) <- player_cash.(cash) + 15)
      cash

(* Update the win count for the winner(s) of each round *)
let update_wins winners =
  List.iter
    (fun (player, _) -> player_wins.(player) <- player_wins.(player) + 1)
    winners

(* Output the wins per player at the end of each round *)
let print_wins () =
  print_endline "Wins per player:";
  for player = 1 to num_players do
    Printf.printf "Player %d: %d\n" player player_wins.(player)
  done;
  print_endline "";
  print_endline "Cash per player:";
  for player = 1 to num_players - 1 do
    Printf.printf "Player %d: %d\n" player player_cash.(player)
  done

let rec clear_screen () =
  let () = print_endline "" in
  let () = print_endline "" in
  let () =
    print_endline
    "Would you like to clear the screen?";
    print_endline
      "Saying yes won't lose any of your information but will just clear up the screen for better visibility";
    print_endline
      "";
    print_endline "Please enter 'y' or 'n'."
  in
  let the_input = read_line () in
  match String.lowercase_ascii the_input with
  | "y" -> clear_terminal ()
  | "n" -> ()
  | _ ->
      print_endline "";
      print_endline "Invalid response, please type either 'y' or 'n'.";
      clear_screen ()

let () =
  get_player_names num_players 1;
  (* clear_terminal (); *)
  print_endline ("Player " ^ string_of_int num_players ^ " will be the dealer");
  print_endline "";
  print_endline
    "Come one come all you gambling addicts. If you're not one yet don't \
     worry... you're about to be :";
  clear_screen ()

let rec print_list = function
  | [] -> ()
  | [ x ] -> print_endline (Card.string_of_card x)
  | x :: xs ->
      print_endline (Card.string_of_card x);
      print_list xs

let print_list_of_tuples lst =
  let rec print_tuple = function
    | [] -> ()
    | (index, sum) :: rest ->
        print_endline
          ("Player: " ^ string_of_int index ^ ", Sum: " ^ string_of_int sum);
        print_tuple rest
  in
  print_tuple lst

let rec get_sum hand count check_sum =
  if count >= List.length hand then check_sum
  else if count = num_players - 1 then
    let cards = List.nth hand count in
    let dealer_first = List.nth cards 0 in
    let sum = Card.hand_value [ dealer_first ] in
    let tuple_app = (count + 1, sum) in
    get_sum hand (count + 1) (List.append check_sum [ tuple_app ])
  else
    let cards = List.nth hand count in
    let sum = Card.hand_value cards in
    let tuple_app = (count + 1, sum) in
    get_sum hand (count + 1) (List.append check_sum [ tuple_app ])

let rec get_sum_after hand count check_sum =
  if count >= List.length hand then check_sum
  else
    let cards = List.nth hand count in
    let sum = Card.hand_value cards in
    let tuple_app = (count + 1, sum) in
    get_sum_after hand (count + 1) (List.append check_sum [ tuple_app ])

let deck = Card.create_deck ()
let used = ref (Card.empty_card ())

(* let used = Player.add_used_card () *)
let hand = ref (Card.deal_cards deck used num_players 1 [])
let () = Card.card_to_player num_players !hand 1 num_players
let () = print_list_of_tuples (get_sum !hand 0 [])

let rec replace_at lst index new_value =
  match lst with
  | [] -> failwith "Index out of bounds"
  | hd :: tl ->
      if index = 0 then new_value :: tl
      else hd :: replace_at tl (index - 1) new_value

let rec get_cards hand count check_sum =
  if count >= List.length hand then check_sum
  else
    let cards = List.nth hand count in
    let sum = Card.hand_value cards in
    if sum <= 21 then
      let tuple_app = (count + 1, sum) in
      get_cards hand (count + 1) (List.append check_sum [ tuple_app ])
    else get_cards hand (count + 1) check_sum

let rec find_max_tuple list max_tuple =
  match list with
  | [] -> max_tuple
  | (x, y) :: rest ->
      if y > snd max_tuple then find_max_tuple rest (x, y)
      else find_max_tuple rest max_tuple

let choose_winner hand count =
  let lst = get_cards !hand count [] in
  let max_value = snd (find_max_tuple lst (0, 0)) in
  let max_tuples = ref [] in
  (* List to store all tuples with maximum value *)
  List.iter
    (fun (player, value) ->
      if value = max_value then max_tuples := (player, value) :: !max_tuples)
    lst;
  match !max_tuples with
  | [] -> [ (0, 0) ] (* Return a default value in case of no winners *)
  | (_, 0) :: _ -> [ (0, 0) ] (* Return a default value if the dealer wins *)
  | _ -> !max_tuples

let save_output filename output =
  let channel = open_out filename in
  output_string channel output;
  close_out channel;
  print_endline ("Output saved to file: " ^ filename)

let print_wins_and_winner () =
  let buffer = Buffer.create 100 in
  Buffer.add_string buffer "Wins per player:\n";
  for player = 1 to num_players do
    Printf.bprintf buffer "Player %d: %d\n" player player_wins.(player)
  done;
  Buffer.add_string buffer "\nCash per player:\n";
  for player = 1 to num_players - 1 do
    Printf.bprintf buffer "Player %d: %d\n" player player_cash.(player)
  done;
  let max_wins = ref 0 in
  let winning_player = ref 0 in
  for player = 1 to num_players do
    if player_wins.(player) > !max_wins then (
      max_wins := player_wins.(player);
      winning_player := player)
  done;
  Buffer.add_string buffer
    ("Player " ^ string_of_int !winning_player ^ " is the winner!\n");
  Buffer.add_string buffer "Winner winner chicken dinner!\n";
  let output = Buffer.contents buffer in
  save_output "output.txt" output

let total_dealer_sum = ref 0

let print_winner () =
  let max_wins = ref 0 in
  let winning_player = ref 0 in
  for player = 1 to num_players do
    if player_wins.(player) > !max_wins then (
      max_wins := player_wins.(player);
      winning_player := player)
  done;
  print_endline ("Player " ^ string_of_int !winning_player ^ " is the winner!");
  print_endline "Winner winner chicken dinner!"

let rec game_loop f round_count players =
  print_endline
    ("Round "
    ^ string_of_int (round_count + 1)
    ^ ": Would you like to play another game of Blackjack?");
  let the_input = read_line () in
  match String.lowercase_ascii the_input with
  | "y" ->
      print_endline "";
      let deck = Card.create_deck () in
      let used = ref (Card.empty_card ()) in
      (* Reset used cards *)
      let hand = ref (Card.deal_cards deck used num_players 1 []) in
      (* Deal new cards *)
      let () = Card.card_to_player num_players !hand 1 num_players in
      let () = print_list_of_tuples (get_sum !hand 0 []) in
      f 1 hand (round_count + 1);
      (* Start the game *)
      game_loop f (round_count + 1) players (* Continue the game loop *)
  | "n" ->
      print_endline "";
      print_endline "Thank you for playing! Goodbye!";
      print_wins ();
      print_endline ("Dealer Cash: " ^ string_of_int(!total_dealer_sum));
      print_endline "";
      print_winner ();
      prompt_save_results () (* Ask user if they want to save results *)
  | _ ->
      print_endline "Invalid response, please type either 'y' or 'n'.";
      game_loop f round_count players (* Repeat the prompt *)

and prompt_save_results () =
  print_endline "Would you like to save the game results?";
  let the_input = read_line () in
  match String.lowercase_ascii the_input with
  | "y" ->
      print_endline "Results saved!";
      print_wins_and_winner();
      print_endline "";
      exit 0
  | "n" ->
      print_endline "Goodbye!";
      print_endline "";
      exit 0
  | _ ->
      print_endline "";
      print_endline "Invalid response, please type either 'y' or 'n'.";
      prompt_save_results () (* Repeat prompt *)

let check_21 (tlist : (int * int) list) =
  match tlist with
  | [] -> false
  | (_, value) :: _ -> if value = 21 then true else false

let check_dealer (tlist : (int * int) list) =
  match tlist with
  | [] -> true
  | (value, _) :: _ -> if value = num_players then false else true

let dealer_money winners =
  if List.length winners = num_players - 1 && check_dealer winners then 0
  else if List.length winners = num_players then 0
  else if not (check_dealer winners) then
    10 * (num_players - List.length winners)
  else 10 * (num_players - 1 - List.length winners)


let rec continue_play count hand c =
  print_endline "";
  print_endline "";
  if count > num_players then (
    print_endline "All Done :)";
    print_endline "";
    print_list_of_tuples (get_sum_after !hand 0 []);
    print_endline "";

    let winners = choose_winner hand 0 in
    print_endline "The winners of this round are:";
    print_list_of_tuples winners;
    update_wins winners;
    if check_21 winners then update_cash winners true
    else update_cash winners false;

    (* Update the win count *)
    print_endline "";
    print_wins ();
    total_dealer_sum := !total_dealer_sum + dealer_money winners;
    print_endline ("Dealer Cash: " ^ string_of_int !total_dealer_sum);
    print_endline "";
    (* Output wins per player *)
    game_loop continue_play c []
    (* Start a new round *))
  else if count < num_players then (
    print_endline
      ("Player " ^ string_of_int count ^ ": would you like to hit or stand?");
    match read_line () with
    | "hit" ->
        let cards = Player.extract !hand (count - 1) in
        print_endline "You chose to hit.";
        let new_hand = Player.hit cards deck used in
        let _ = Card.print_card_list new_hand in
        hand := replace_at !hand (count - 1) new_hand;
        let sum = Card.hand_value new_hand in
        if Player.bust sum then (
          print_endline "Your sum is greater than 21! You bust!";
          continue_play (count + 1) hand c)
        else (
          print_endline ("Your new sum is: " ^ string_of_int sum);
          game count hand c)
    | "stand" ->
        print_endline "All right, moving on.";
        continue_play (count + 1) hand c
    | _ ->
        print_endline "Invalid response, please type either 'hit' or 'stand'.";
        continue_play count hand c)
  else
    let () = print_endline "Dealer's turn." in
    let rec dealer_play () =
      let dealer_hand = Player.extract !hand (num_players - 1) in
      let dealer_sum_before = Card.hand_value dealer_hand in
      let all_bust = Array.fold_right ( + ) busted 0 in
      if all_bust = num_players - 1 then (
        print_endline "Dealer stands.";
        let _ = Card.print_card_list dealer_hand in
        continue_play (count + 1) hand c)
      else if dealer_sum_before < 17 then begin
        print_endline "Dealer hits.";
        let new_dealer_hand = Player.hit dealer_hand deck used in
        let _ = Card.print_card_list new_dealer_hand in
        hand := replace_at !hand (num_players - 1) new_dealer_hand;
        let dealer_sum_after = Card.hand_value new_dealer_hand in
        (* Print after *)
        print_endline
          ("Dealer's sum after hitting: " ^ string_of_int dealer_sum_after);
        if Player.bust dealer_sum_after then (
          print_endline "Dealer busts!";
          let _ = Card.print_card_list new_dealer_hand in
          continue_play (count + 1) hand c)
        else dealer_play ()
      end
      else begin
        print_endline "Dealer stands.";
        let _ = Card.print_card_list dealer_hand in
        continue_play (count + 1) hand c
      end
    in
    dealer_play ()

and game count hand c =
  print_endline "";
  (* Empty lines for readability *)
  if count > num_players then print_endline "All Done :)"
  else begin
    print_endline
      ("Player " ^ string_of_int count ^ ": would you like to hit or stand?");
    match read_line () with
    | "hit" ->
        let cards = Player.extract !hand (count - 1) in
        let new_hand = Player.hit cards deck used in
        print_endline "You chose to hit.";
        let _ = Card.print_card_list new_hand in
        hand := replace_at !hand (count - 1) new_hand;
        let sum = Card.hand_value new_hand in
        if Player.bust sum then (
          busted.(count) <- 1;
          print_endline "Your sum is greater than 21! You bust!";
          continue_play (count + 1) hand c)
        else (
          print_endline ("Your new sum is: " ^ string_of_int sum);
          game count hand c)
    | "stand" ->
        print_endline "All right, moving on.";
        continue_play (count + 1) hand c
    | _ ->
        print_endline "Invalid response, please type either 'hit' or 'stand'.";
        game count hand c
  end

let () =
  continue_play 1 hand 1;
  print_winner ()
