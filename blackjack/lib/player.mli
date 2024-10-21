(* @authors Sruthi Suresh (sls463), Advika Ravi Kumar (ar732), 
   Aaradhyaa Gyawali (ag2223), Ishaan Nanal (isn6) *)
   
open Card
(** Player module *)
module Player : sig



(* player.mli *)

(** The type representing a player's hand. *)
type hand = Card.card list

(** The type representing a player's score. *)
type score = int

val bust : int -> bool
(** [bust sum] checks if the player has busted, i.e., if their total score is greater than 21. *)

val extract : Card.card list list -> int -> Card.card list
(** [extract hand_list num] returns the [num]-th hand from [hand_list].
    Requires: [num] to be a valid index within [hand_list]. *)

val hit : hand -> Card.card list -> Card.card list ref -> Card.card list
(** [hit hand deck used] simulates the player hitting for a new card. Adds a
    randomly selected card from [deck] to the player's [hand]. Requires: [deck]
    to be a valid deck of cards and [used] to be a reference to the list of
    cards already used. *)
end 