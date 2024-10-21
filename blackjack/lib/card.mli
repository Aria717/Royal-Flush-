(* @authors Sruthi Suresh (sls463), Advika Ravi Kumar (ar732), 
   Aaradhyaa Gyawali (ag2223), Ishaan Nanal (isn6) *)
module Card : sig
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

  val print_card : card -> unit  
  (** [print_card c] prints a string representation of the given card [c].
    @param c The card to be printed. *)
  
  val print_card_list : card list-> unit  
  (** [print_card c] prints a string representation of the given card [c].
    @param c The card to be printed. *)

  val empty_card : unit -> card list
  (** [empty_card ()] returns an empty list of cards.
    @return An empty list representing no cards. *)

  val create_deck : unit -> card list
  (** [create_deck ()] generates a standard deck of 52 cards. 
       @return A list of cards representing a standard deck. *)

  val string_of_suit : suit -> string
  (** [string_of_suit s] returns a string representation of the given suit [s]. 
    @param s The suit to be converted to a string.
    @return A string representation of the suit [s].*)

  val string_of_rank : rank -> string
  (** [string_of_rank r] returns a string representation of the given rank [r]. 
    @param r The rank to be converted to a string.
    @return A string representation of the rank [r]. *)

  val string_of_card : card -> string
  (** [string_of_card c] returns a string representation of the given card [c].
    @param c The card to be converted to a string.
    @return A string representation of the card [c]. *)

  val pick_rand_card : card list -> card list ref -> card
  (** [pick_rand_card deck used] selects a random card from the [deck] and adds it to the list of used cards [used].
    @param deck The list of available cards.
    @param used A reference to the list of cards that have been used.
    @return A randomly selected card from the deck. *)


  val deal_cards :
    card list -> card list ref -> int -> int -> card list list -> card list list
    (** [deal_cards deck used num_players count hand] deals a specified number of cards to each player.
    @param deck The list of available cards.
    @param used A reference to the list of cards that have been used.
    @param num_players The number of players in the game.
    @param count The number of cards to deal to each player.
    @param hand The current hands of the players.
    @return An updated list of hands with the newly dealt cards. *)


  val card_to_player : int -> card list list -> int -> int  -> unit
  (** [card_to_player num_players cards count value] prints out the cards dealt to each player.
    @param num_players The number of players in the game.
    @param cards The current hands of the players.
    @param count The number of the player whose cards are being printed.
    @param value The total number of players. *)



  val hand_value : card list -> int
  (** [hand_value hand] calculates the total value of a hand of cards.
    @param hand A list of cards representing the player's hand.
    @return The total value of the hand. *)


  val total : card list list -> int -> int
  (** [total hands player_num] calculates the total value of a hand for a specific player.
    @param hands A list of hands for all players.
    @param player_num The number of the player whose hand total is being calculated.
    @return The total value of the player's hand. *)


end
